/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2022 The Stockfish developers (see AUTHORS file)

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef BITBOARD_H_INCLUDED
#define BITBOARD_H_INCLUDED

#include <string>

#include "types.h"

namespace Stockfish {

namespace Bitboards {

void init();
std::string pretty(Bitboard b);

} // namespace Stockfish::Bitboards
//位棋盘定义 将 所在的九宫格
constexpr Bitboard Palace = (Bitboard(0xAAA555AAA555AAULL) << 64) ^ Bitboard(0xA555AAA555AAA555ULL);
//位棋盘定义第一列的值,其它列位棋盘通过位移方式实现，类似
/*
 * 1
 * 1
 * 1
 * 1
 * 1
 * 1
 * 1
 * 1
 * 1
 * 1
 */
constexpr Bitboard FileABB = (Bitboard(0x00100100100100ULL) << 64) ^ Bitboard(0x1001001001001001ULL);
constexpr Bitboard FileBBB = FileABB << 1;
constexpr Bitboard FileCBB = FileABB << 2;
constexpr Bitboard FileDBB = FileABB << 3;
constexpr Bitboard FileEBB = FileABB << 4;
constexpr Bitboard FileFBB = FileABB << 5;
constexpr Bitboard FileGBB = FileABB << 6;
constexpr Bitboard FileHBB = FileABB << 7;
constexpr Bitboard FileIBB = FileABB << 8;
//
//位棋盘定义第一行的值,其它行位棋盘通过位移方式实现，类似
/*
 * 1 1 1 1 1 1 1 1 1
 */
constexpr Bitboard Rank0BB = 0x1FF;
constexpr Bitboard Rank1BB = Rank0BB << (FILE_NB * 1);
constexpr Bitboard Rank2BB = Rank0BB << (FILE_NB * 2);
constexpr Bitboard Rank3BB = Rank0BB << (FILE_NB * 3);
constexpr Bitboard Rank4BB = Rank0BB << (FILE_NB * 4);
constexpr Bitboard Rank5BB = Rank0BB << (FILE_NB * 5);
constexpr Bitboard Rank6BB = Rank0BB << (FILE_NB * 6);
constexpr Bitboard Rank7BB = Rank0BB << (FILE_NB * 7);
constexpr Bitboard Rank8BB = Rank0BB << (FILE_NB * 8);
constexpr Bitboard Rank9BB = Rank0BB << (FILE_NB * 9);

//兵的所在位置，只能是第一列，第三列，第五列，第七列，第九列
constexpr Bitboard PawnFileBB = FileABB | FileCBB | FileEBB | FileGBB | FileIBB;
//红方黑方一半棋盘，分别是前五行和后五行
constexpr Bitboard HalfBB[2] = { Rank0BB | Rank1BB | Rank2BB | Rank3BB | Rank4BB,
                                 Rank5BB | Rank6BB | Rank7BB | Rank8BB | Rank9BB };
//计算兵的最终能走的位置
constexpr Bitboard PawnBB[2] = { HalfBB[BLACK] | ((Rank3BB | Rank4BB) & PawnFileBB),
                                 HalfBB[WHITE] | ((Rank6BB | Rank5BB) & PawnFileBB) };
//用于存储位值等于1的数组
extern uint8_t PopCnt16[1 << 16];
//提前计算存储两个棋子之间距离
extern uint8_t SquareDistance[90][90];
//存储棋子
extern Bitboard SquareBB[90];
//主要用于计算将军的blocker
extern Bitboard BetweenBB[90][90];

extern Bitboard LineBB[90][90];


extern Bitboard PseudoAttacks[8][90];
//存储兵的攻击位置
extern Bitboard PawnAttacks[2][90];
//兵被攻击的位置
extern Bitboard PawnAttacksTo[2][90];

int popcount(Bitboard b); // required for 128 bit pext

/// Magic 位棋盘，用于计算位置索引，具体参考magic bitboard
struct Magic {
  Bitboard  mask;
  Bitboard  magic;
  Bitboard* attacks;
  unsigned  shift;

  // Compute the attack's index using the 'magic bitboards' approach
  unsigned index(Bitboard occupied) const {

    if (HasPext)
        return unsigned(pext(occupied, mask, shift));

    return unsigned(((occupied & mask) * magic) >> shift);
  }
};

extern Magic RookMagics[SQUARE_NB];
extern Magic CannonMagics[SQUARE_NB];
extern Magic BishopMagics[SQUARE_NB];
extern Magic KnightMagics[SQUARE_NB];
extern Magic KnightToMagics[SQUARE_NB];

inline Bitboard square_bb(Square s) {
  assert(is_ok(s));
  return SquareBB[s];
}
//以下是bitboard运算符重载

/// Overloads of bitwise operators between a Bitboard and a Square for testing
/// whether a given bit is set in a bitboard, and for setting and clearing bits.

inline Bitboard  operator&( Bitboard  b, Square s) { return b &  square_bb(s); }
inline Bitboard  operator|( Bitboard  b, Square s) { return b |  square_bb(s); }
inline Bitboard  operator^( Bitboard  b, Square s) { return b ^  square_bb(s); }
inline Bitboard& operator|=(Bitboard& b, Square s) { return b |= square_bb(s); }
inline Bitboard& operator^=(Bitboard& b, Square s) { return b ^= square_bb(s); }

inline Bitboard  operator&(Square s, Bitboard b) { return b & s; }
inline Bitboard  operator|(Square s, Bitboard b) { return b | s; }
inline Bitboard  operator^(Square s, Bitboard b) { return b ^ s; }

inline Bitboard  operator|(Square s1, Square s2) { return square_bb(s1) | s2; }

//位棋盘上的棋子数量是否大于1
constexpr bool more_than_one(Bitboard b) {
  return b & (b - 1);
}


/// rank_bb() and file_bb() return a bitboard representing all the squares on
/// the given file or rank.
//获取某一行的位棋盘
constexpr Bitboard rank_bb(Rank r) {
  return Rank0BB << (FILE_NB * r);
}
//获取某一行的位棋盘
constexpr Bitboard rank_bb(Square s) {
  return rank_bb(rank_of(s));
}
//获取某一列的位棋盘
constexpr Bitboard file_bb(File f) {
  return FileABB << f;
}
//获取某一列的位棋盘
constexpr Bitboard file_bb(Square s) {
  return file_bb(file_of(s));
}


/// shift() moves a bitboard one or two steps as specified by the direction D
//移动棋子
template<Direction D>
constexpr Bitboard shift(Bitboard b) {
  return  D == NORTH      ? (b & ~Rank9BB           ) << NORTH       : D == SOUTH      ?  b             >> NORTH
        : D == NORTH+NORTH? (b & ~Rank9BB & ~Rank8BB) << NORTH+NORTH : D == SOUTH+SOUTH?  b             >> NORTH+NORTH
        : D == EAST       ? (b & ~FileIBB           ) << EAST        : D == WEST       ? (b & ~FileABB) >> EAST
        : D == NORTH_EAST ? (b & ~FileIBB           ) << NORTH_EAST  : D == NORTH_WEST ? (b & ~FileABB) << NORTH_WEST
        : D == SOUTH_EAST ? (b & ~FileIBB           ) >> NORTH_WEST  : D == SOUTH_WEST ? (b & ~FileABB) >> NORTH_EAST
        : Bitboard(0);
}


/// pawn_attacks_bb() returns the squares attacked by pawns of the given color
/// from the squares in the given bitboard.

template<Color C>
constexpr Bitboard pawn_attacks_bb(Square s) {
  Bitboard b = square_bb(s);
  Bitboard attack;
  if(C == WHITE){
      attack= shift<NORTH>(b);
  }else{
      attack= shift<SOUTH>(b);
  }

  if ((C == WHITE && rank_of(s) > RANK_4) || (C == BLACK && rank_of(s) < RANK_5)){
      attack=attack| shift<WEST>(b) | shift<EAST>(b);
  }

  return attack;
}

inline Bitboard pawn_attacks_bb(Color c, Square s) {

  return PawnAttacks[c][s];
}


/// pawn_attacks_to_bb() returns the squares that if there is a pawn
/// of the given color in there, it can attack the square s

template<Color C>
constexpr Bitboard pawn_attacks_to_bb(Square s) {
  Bitboard b = square_bb(s);
  Bitboard attack;
  if(C == WHITE){
      attack = shift< SOUTH>(b);
  }else{
      attack = shift< NORTH>(b);
  }

  if ((C == WHITE && rank_of(s) > RANK_4) || (C == BLACK && rank_of(s) < RANK_5)){
      attack =attack| shift<WEST>(b) | shift<EAST>(b);
  }

  return attack;
}

inline Bitboard pawn_attacks_to_bb(Color c, Square s) {
  return PawnAttacksTo[c][s];
}


/// line_bb() returns a bitboard representing an entire line (from board edge
/// to board edge) that intersects the two given squares. If the given squares
/// are not on a same file/rank/diagonal, the function returns 0. For instance,
/// line_bb(SQ_C4, SQ_F7) will return a bitboard with the A2-G8 diagonal.

inline Bitboard line_bb(Square s1, Square s2) {

  return LineBB[s1][s2];
}


/// between_bb(s1, s2) returns a bitboard representing the squares in the semi-open
/// segment between the squares s1 and s2 (excluding s1 but including s2). If the
/// given squares are not on a same file/rank/diagonal, it returns s2. For instance,
/// between_bb(SQ_C4, SQ_F7) will return a bitboard with squares D5, E6 and F7, but
/// between_bb(SQ_E6, SQ_F8) will return a bitboard with the square F8. This trick
/// allows to generate non-king evasion moves faster: the defending piece must either
/// interpose itself to cover the check or capture the checking piece.

inline Bitboard between_bb(Square s1, Square s2) {

  return BetweenBB[s1][s2];
}


/// aligned() returns true if the squares s1, s2 and s3 are aligned either on a
/// straight or on a diagonal line.

inline bool aligned(Square s1, Square s2, Square s3) {
  return line_bb(s1, s2) & s3;
}


/// distance() functions return the distance between x and y, defined as the
/// number of steps for a king in x to reach y.

template<typename T1 = Square> inline int distance(Square x, Square y);
template<> inline int distance<File>(Square x, Square y) {
    return std::abs(file_of(x) - file_of(y));
}
template<> inline int distance<Rank>(Square x, Square y) {
    return std::abs(rank_of(x) - rank_of(y));
}
template<> inline int distance<Square>(Square x, Square y) {
    return SquareDistance[x][y];
}



/// attacks_bb(Square) returns the pseudo attacks of the give piece type
/// assuming an empty board.

template<PieceType Pt>
inline Bitboard attacks_bb(Square s) {

  return PseudoAttacks[Pt][s];
}


/// attacks_bb(Square, Bitboard) returns the attacks by the given piece
/// assuming the board is occupied according to the passed Bitboard.
/// Sliding piece attacks do not continue passed an occupied square.

template<PieceType Pt>
inline Bitboard attacks_bb(Square s, Bitboard occupied) {
      if(Pt==ROOK){
          return     RookMagics[s].attacks[    RookMagics[s].index(occupied)];
      } else if(Pt==CANNON){
          return   CannonMagics[s].attacks[  CannonMagics[s].index(occupied)];
      }else if(Pt==BISHOP){
          return   BishopMagics[s].attacks[  BishopMagics[s].index(occupied)];
      }else if(Pt==KNIGHT){
          return   KnightMagics[s].attacks[  KnightMagics[s].index(occupied)];
      }else if(Pt==KNIGHT_TO){
          return KnightToMagics[s].attacks[KnightToMagics[s].index(occupied)];
      }else{
          return PseudoAttacks[Pt][s];
      }
}

inline Bitboard attacks_bb(PieceType pt, Square s, Bitboard occupied) {
    if(pt==ROOK){
        return attacks_bb<  ROOK>(s, occupied);
    }else if(pt==CANNON){
        return attacks_bb<CANNON>(s, occupied);
    }else if(pt==BISHOP){
        return attacks_bb<BISHOP>(s, occupied);
    }else if(pt==KNIGHT){
        return attacks_bb<KNIGHT>(s, occupied);
    }else{
        return PseudoAttacks[pt][s];
    }
}


/// popcount() counts the number of non-zero bits in a bitboard

inline int popcount(Bitboard b) {

#ifndef USE_POPCNT

  union { Bitboard bb; uint16_t u[8]; } v = { b };
  return  PopCnt16[v.u[0]] + PopCnt16[v.u[1]] + PopCnt16[v.u[2]] + PopCnt16[v.u[3]]
         + PopCnt16[v.u[4]] + PopCnt16[v.u[5]] + PopCnt16[v.u[6]] + PopCnt16[v.u[7]];

#elif defined(_MSC_VER) || defined(__INTEL_COMPILER)

    return (int)_mm_popcnt_u64(b >> 64) + (int)_mm_popcnt_u64(b);

#else // Assumed gcc or compatible compiler

  return __builtin_popcountll(b >> 64) + __builtin_popcountll(b);

#endif
}


/// lsb() return the least significant bit in a non-zero bitboard

inline Square lsb(Bitboard b) {
  assert(b);

#if defined(_MSC_VER) // MSVC

  unsigned long idx;
  if (uint64_t(b))
  {
      _BitScanForward64(&idx, b);
      return Square(idx);
  }
  else
  {
      _BitScanForward64(&idx, b >> 64);
      return Square(idx + 64);
  }

#else // Assumed gcc or compatible compiler

    if (uint64_t(b))
        return Square(__builtin_ctzll(b));
    return Square(__builtin_ctzll(b >> 64) + 64);

#endif
}

/// least_significant_square_bb() returns the bitboard of the least significant
/// square of a non-zero bitboard. It is equivalent to square_bb(lsb(bb)).

inline Bitboard least_significant_square_bb(Bitboard b) {
  assert(b);
  return b & -b;
}

/// pop_lsb() finds and clears the least significant bit in a non-zero bitboard

inline Square pop_lsb(Bitboard& b) {
  assert(b);
  const Square s = lsb(b);
  b &= b - 1;
  return s;
}

} // namespace Stockfish

#endif // #ifndef BITBOARD_H_INCLUDED
