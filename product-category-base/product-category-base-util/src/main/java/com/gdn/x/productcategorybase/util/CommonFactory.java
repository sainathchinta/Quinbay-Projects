package com.gdn.x.productcategorybase.util;

import org.springframework.stereotype.Component;
import org.supercsv.io.CsvBeanReader;
import org.supercsv.io.CsvBeanWriter;
import org.supercsv.prefs.CsvPreference;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

/**
 * Created by arie.prastowo on 9/29/2016.
 */
@Component
public class CommonFactory implements ICommonFactory {
    public CsvBeanReader createCsvBeanReader(InputStream inputStream, String encoding)
            throws UnsupportedEncodingException {
        return new CsvBeanReader(new InputStreamReader(inputStream, encoding),
                CsvPreference.STANDARD_PREFERENCE);
    }
    public CsvBeanWriter createCsvBeanWriter(Writer writer) {
        return new CsvBeanWriter(writer, CsvPreference.STANDARD_PREFERENCE);
    }
}
