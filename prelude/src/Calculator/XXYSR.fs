namespace Prelude.Calculator

open Prelude
open Prelude.Charts
open Percyqaz.Common
open StarRatingRebirth

module XXY_SR =

    let notes_to_xxydata (notes: TimeArray<NoteRow>) (keys: int) : ManiaData =
        let rec ln_lookahead k (snaps: TimeItem<NoteRow> list) =
            match snaps with
            | { Time = offset; Data = nr } :: ss ->
                if nr.[k] = NoteType.HOLDTAIL then
                    offset
                else
                    ln_lookahead k ss
            | [] -> failwith "hold note has no end"

        let rec convert (snaps: TimeItem<NoteRow> list) =
            seq {
                match snaps with
                | { Time = offset; Data = nr } :: ss ->
                    for k = 0 to keys - 1 do
                        if nr.[k] = NoteType.NORMAL then
                            yield Note(k, int offset, -1)
                        elif nr.[k] = NoteType.HOLDHEAD then
                            yield Note(k, int offset, int (ln_lookahead k ss))

                    yield! convert ss
                | [] -> ()
            }

        let data = ManiaData()
        data.CS <- keys
        data.OD <- 8.0
        data.Notes <- ResizeArray(convert (notes |> Array.toList))
        data

    let calculate_xxysr (rate: Rate, notes: TimeArray<NoteRow>) : float32 =
        let data = (notes_to_xxydata notes notes[0].Data.Length).ChangeRate(1.0 / float rate)
        try
            SRCalculator.Calculate(data) |> single
        with
        | ex ->
            Logging.Info "SRCalculator.Calculate failed: %s" ex.Message
            0.0f