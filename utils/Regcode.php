<?php
function GenerateRegistrationCode($seed, $prime, $hotsyncId) {

        $code = 65535;
        $code = $seed == 0 ? 49151 << 5 : $seed;

        # original C
        #code += uid[j]*uid[j]*(prime*j*(j+1)+2579*prime);

        for ($j=0; $j< strlen($hotsyncId);$j++) {
                $ordv = ord($hotsyncId[$j]);
                $code += ($ordv*$ordv)*($prime*$j*($j+1)+2579*$prime);
        }
        #print (fmod($code,4294967296))."\n";
        return fmod($code,4294967296);

}

function GenerateDemoCode($prime, $hotsyncId, $year,$month,$day) {

        $dateMask = 0xFFC00FFF;
        $dateBits = (($year-2009)<<9)|($month<<5)|$day;

        print $dateBits."\n";
        $code = GenerateRegistrationCode(0,$prime,$hotsyncId) & $dateMask | ($dateBits << 12);

        return $code < 0 ? $code + 4294967296 : $code;
}
?>
