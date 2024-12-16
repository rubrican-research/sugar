unit markdownhelper;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;
type
    NMDMarker = (
        mdText,
        mdH1,
        mdH2,
        mdH3,
        mdH4,
        mdH5,
        mdUL,
        mdOL,
        mdTask,
        mdCode,
        mdBlockQuote,
        mdLink,
        mdImage,
        mdFrame,
        mdBold,
        mdItalic,
        mdUnderline,
        mdStrikeThrough,
        mdGeneric
    );

const
    MDPrefix: array[NMDMarker] of string = (
                '',        // mdText,
                '#',       // mdH1,
                '##',      // mdH2,
                '###',     // mdH3,
                '####',    // mdH4,
                '####',    // mdH5,
                '-',       // mdUL,
                '1.',      // mdOL,
                '- [ ]',   // mdTask,
                '`',       // mdCode,
                '', // mdBlockQuote,
                '', // mdLink,
                '', // mdImage,
                '', // mdFrame,
                '', // mdBold,
                '', // mdItalic,
                '', // mdUnderline,
                '', // mdStrikeThrough,
                ''  // mdGeneric
    );



implementation

end.

