package app

enum ShapeSignature(signature : (
                                    Int, Int, Int,
                                    Int, Int, Int,
                                    Int, Int, Int
                                )) {
    case DOT extends ShapeSignature(
        0, 0, 0,
        0, 1, 0,
        0, 0, 0
    )
    
    case SQUARE extends ShapeSignature(
        0, 0, 0,
        0, 1, 1,
        0, 1, 1
    )

    case T extends ShapeSignature(
        0, 0, 0,
        1, 1, 1,
        0, 1, 0
    )

    case L extends ShapeSignature(
        0, 1, 0,
        0, 1, 0,
        0, 1, 1
    )
    
    case REVERSE_L extends ShapeSignature(
        0, 1, 0,
        0, 1, 0,
        1, 1, 0
    )

    case BAR extends ShapeSignature(
        0, 1, 0,
        0, 1, 0,
        0, 1, 0
    )
}