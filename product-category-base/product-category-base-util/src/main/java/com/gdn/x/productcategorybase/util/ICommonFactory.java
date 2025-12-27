package com.gdn.x.productcategorybase.util;

import org.supercsv.io.CsvBeanReader;
import org.supercsv.io.CsvBeanWriter;

import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

/**
 * Created by arie.prastowo on 9/29/2016.
 */
public interface ICommonFactory {
    CsvBeanReader createCsvBeanReader(InputStream inputStream, String encoding)
            throws UnsupportedEncodingException;
    CsvBeanWriter createCsvBeanWriter(Writer writer);
}
