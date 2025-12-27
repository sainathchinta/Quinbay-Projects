package com.gdn.mta.bulk.util;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.models.FileType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.x.mta.rest.web.request.ProductApiGeneralAttributeRequest;


public class StreamUtilTest {
  
  @InjectMocks
  private StreamUtils instance;
  
  @BeforeEach
  public void setup(){
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testDistinctByKey() {
    List<ProductApiGeneralAttributeRequest> productApiGeneralAttributeRequestTest = new ArrayList<ProductApiGeneralAttributeRequest>();
    productApiGeneralAttributeRequestTest.add(new ProductApiGeneralAttributeRequest("a", "v"));
    productApiGeneralAttributeRequestTest.add(new ProductApiGeneralAttributeRequest("a", "v"));
    productApiGeneralAttributeRequestTest.add(new ProductApiGeneralAttributeRequest("b", "v"));
    productApiGeneralAttributeRequestTest.add(new ProductApiGeneralAttributeRequest("a", "v"));
    productApiGeneralAttributeRequestTest.add(new ProductApiGeneralAttributeRequest("a", "v"));
    List<ProductApiGeneralAttributeRequest> request  = productApiGeneralAttributeRequestTest.stream().filter(StreamUtils.distinctByKey(test -> ((ProductApiGeneralAttributeRequest) test).getName())).collect(Collectors.toList());
    Assertions.assertEquals(2, request.size());
  }

  @Test
  public void processorUtilsFindFiles() throws Exception {
    ProcessorUtils.findAllFiles("target");
  }

  @Test
  public void processorUtilsgetFileFormat() throws Exception {
    String result = ProcessorUtils.getFileFormat("abc.xlsx");
    Assertions.assertEquals(".xlsx", result);
  }

  @Test
  public void processorUtilsgetFileFormatXls() throws Exception {
    String result = ProcessorUtils.getFileFormat("abc.xls");
    Assertions.assertEquals(".xls", result);
  }

  @Test
  public void processorUtilsgetFileFormatXlsm() throws Exception {
    String result = ProcessorUtils.getFileFormat("abc.xlsm");
    Assertions.assertEquals(".xlsm", result);
  }

  @Test
  public void processorUtilsgetFileFormatCSV() throws Exception {
    String result = ProcessorUtils.getFileFormat("abc.csv");
    Assertions.assertEquals( ".csv", result);
  }

  @Test
  public void processorUtilsgetFileType() throws Exception {
    FileType result = ProcessorUtils.getFileType("abc.xlsx");
    Assertions.assertEquals(FileType.XLSX, result);
  }

  @Test
  public void processorUtilsgetFileTypeCLS() throws Exception {
    FileType result = ProcessorUtils.getFileType("abc.xls");
    Assertions.assertEquals(FileType.XLS, result);
  }

  @Test
  public void processorUtilsgetFileTypeCSV() throws Exception {
    FileType result = ProcessorUtils.getFileType("abc.csv");
    Assertions.assertEquals(FileType.CSV, result);
  }

  @Test
  public void processorUtilsgetXlsmFileType() throws Exception {
    FileType result = ProcessorUtils.getFileType("abc.xlsm");
    Assertions.assertEquals(FileType.XLSM, result);
  }
}
