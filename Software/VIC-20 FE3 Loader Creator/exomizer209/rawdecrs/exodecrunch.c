/*
 * Copyright (c) 2005 Magnus Lind.
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from
 * the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 *   1. The origin of this software must not be misrepresented * you must not
 *   claim that you wrote the original software. If you use this software in a
 *   product, an acknowledgment in the product documentation would be
 *   appreciated but is not required.
 *
 *   2. Altered source versions must be plainly marked as such, and must not
 *   be misrepresented as being the original software.
 *
 *   3. This notice may not be removed or altered from any distribution.
 *
 *   4. The names of this software and/or it's copyright holders may not be
 *   used to endorse or promote products derived from this software without
 *   specific prior written permission.
 */

#include "exodecrunch.h"
#include <stdlib.h>

enum exo_state
{
    STATE_NEXT_BYTE,
    STATE_NEXT_LITERAL_BYTE,
    STATE_NEXT_SEQUENCE_BYTE,
    STATE_EOF
};

struct exo_table_entry
{
    unsigned char bits;
    unsigned short int base;
};

struct exo_decrunch_ctx
{
    struct exo_table_entry *lengths;
    struct exo_table_entry *offsets1;
    struct exo_table_entry *offsets2;
    struct exo_table_entry *offsets3;
    unsigned char *window;
    unsigned short int window_length;

    enum exo_state state;
    unsigned short int length;
    unsigned short int offset;
    unsigned short int window_pos;
    unsigned short int bit_buffer;

    exo_read_crunched_byte *read_byte;
    void *read_data;
};

static int
read_bits(struct exo_decrunch_ctx *ctx, int bit_count)
{
    int bits = 0;
    while(bit_count-- > 0)
    {
        if(ctx->bit_buffer == 1)
        {
            ctx->bit_buffer = 0x100 | ctx->read_byte(ctx->read_data);
        }
        bits <<= 1;
        bits |= ctx->bit_buffer & 0x1;
        ctx->bit_buffer >>= 1;
    }
    return bits;
}

static struct exo_table_entry *
generate_table(struct exo_decrunch_ctx *ctx, int size)
{
    int base;
    int i;
    struct exo_table_entry *table;
    table = malloc(size * sizeof(struct exo_table_entry));
    base = 1;
    for(i = 0; i < size; ++i)
    {
        table[i].base = base;
        table[i].bits = (unsigned char)read_bits(ctx, 4);
        base += 1 << table[i].bits;
    }
    return table;
}

struct exo_decrunch_ctx *
exo_decrunch_new(unsigned short int max_offset,
                 exo_read_crunched_byte *read_byte,
                 void *read_data)
{
    struct exo_decrunch_ctx *ctx;

    ctx = malloc(sizeof(struct exo_decrunch_ctx));

    ctx->state = STATE_NEXT_BYTE;
    ctx->window_pos = 0;
    ctx->bit_buffer = read_byte(read_data);
    ctx->read_byte = read_byte;
    ctx->read_data = read_data;

    ctx->lengths = generate_table(ctx, 16);
    ctx->offsets3 = generate_table(ctx, 16);
    ctx->offsets2 = generate_table(ctx, 16);
    ctx->offsets1 = generate_table(ctx, 4);
    ctx->window = malloc(max_offset);
    ctx->window_length = max_offset;

    return ctx;
}

static int
get_gamma_code(struct exo_decrunch_ctx *ctx)
{
    int gamma;

    gamma = 0;
    while(read_bits(ctx, 1) == 0)
    {
        ++gamma;
    }
    return gamma;
}

static
int read_byte_from_window(struct exo_decrunch_ctx *ctx, int offset)
{
    int read_pos;

    read_pos = ctx->window_pos - offset;
    if(read_pos < 0)
    {
        read_pos += ctx->window_length;
    }
    return ctx->window[read_pos];
}

int
exo_read_decrunched_byte(struct exo_decrunch_ctx *ctx)
{
    int c;
    int length_index;
    struct exo_table_entry *table_entry;

    switch(ctx->state)
    {
    case STATE_NEXT_BYTE:
        if(read_bits(ctx, 1) == 1)
        {
            /* literal byte */
            c = ctx->read_byte(ctx->read_data);
            break;
        }
        /* sequence */
        length_index = get_gamma_code(ctx);
        if(length_index == 17)
        {
            /* literal data block */
            ctx->length = read_bits(ctx, 16);
            ctx->state = STATE_NEXT_LITERAL_BYTE;
    case STATE_NEXT_LITERAL_BYTE:
            if(--ctx->length == 0)
            {
                ctx->state = STATE_NEXT_BYTE;
            }
            c = ctx->read_byte(ctx->read_data);
            break;
        }
        else if(length_index == 16)
        {
            /* end of data marker, we're done. */
            ctx->state = STATE_EOF;
    case STATE_EOF:
            return -1;
        }
        /* sequence */
        table_entry = ctx->lengths + length_index;
        ctx->length = table_entry->base + read_bits(ctx, table_entry->bits);
        switch(ctx->length)
        {
        case 1:
            table_entry = ctx->offsets1 + read_bits(ctx, 2);
            break;
        case 2:
            table_entry = ctx->offsets2 + read_bits(ctx, 4);
            break;
        default:
            table_entry = ctx->offsets3 + read_bits(ctx, 4);
        }
        ctx->offset = table_entry->base + read_bits(ctx, table_entry->bits);
        ctx->state = STATE_NEXT_SEQUENCE_BYTE;
    case STATE_NEXT_SEQUENCE_BYTE:
        if(--ctx->length == 0)
        {
            ctx->state = STATE_NEXT_BYTE;
        }
        c = read_byte_from_window(ctx, ctx->offset);
        break;
    }

    ctx->window[ctx->window_pos++] = (unsigned char)c;
    if(ctx->window_pos == ctx->window_length)
    {
        ctx->window_pos = 0;
    }
    return c;
}

void
exo_decrunch_delete(struct exo_decrunch_ctx *ctx)
{
    free(ctx->window);
    free(ctx->offsets1);
    free(ctx->offsets2);
    free(ctx->offsets3);
    free(ctx->lengths);
    free(ctx);
}
