package com.gdn.x.productcategorybase.util;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;

import org.junit.jupiter.api.Test;
import org.supercsv.io.CsvBeanReader;
import org.supercsv.io.CsvBeanWriter;

/**
 * Created by arie.prastowo on 9/29/2016.
 */
public class CommonFactoryTest {
    @Test
    public void createCsvBeanReader() throws Exception{
        CommonFactory cf = new CommonFactory();
        CsvBeanReader cbr = cf.createCsvBeanReader(
                new ByteArrayInputStream(new byte[1024]), "UTF-8");
        assertNotNull(cbr);
    }
    @Test
    public void createCsvBeanWriter() {
        CommonFactory cf = new CommonFactory();
        CsvBeanWriter cbw = cf.createCsvBeanWriter(new PrintWriter(new ByteArrayOutputStream(1024)));
        assertNotNull(cbw);
    }
}
