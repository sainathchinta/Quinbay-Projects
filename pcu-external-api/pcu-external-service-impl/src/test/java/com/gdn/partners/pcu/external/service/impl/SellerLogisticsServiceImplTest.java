package com.gdn.partners.pcu.external.service.impl;

import com.gdn.partners.pcu.external.client.feign.SellerLogisticsFeign;
import com.gdn.partners.pcu.external.client.helper.Response;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.seller.logistics.web.model.request.UploadExcelRequest;
import com.gdn.seller.logistics.web.model.response.DownloadSkuTemplateResponse;
import com.gdn.seller.logistics.web.model.response.GetSellerLogisticProductResponse;
import com.gdn.seller.logistics.web.model.response.UploadExcelSkuUpdateResponse;
import com.gdn.seller.logistics.web.model.response.UploadExcelSkuUpdateStatusResponse;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpStatus;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class SellerLogisticsServiceImplTest {
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PICKUP = "PICKUP";
  private static final String ERROR = "error";
  private static final String DUMMY_FILE_NAME = "dummy-excel.xls";
  private static final String FILE_FOLDER = "Product";

  @Mock
  private SellerLogisticsFeign sellerLogisticsFeign;

  @InjectMocks
  private SellerLogisticsServiceImpl sellerLogisticsService;

  private Map<String, List<String>> errorMap;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    errorMap = new HashMap<>();
    errorMap.put(ERROR, Collections.singletonList(ERROR));
    when(sellerLogisticsFeign.getSellerLogisticProducts(BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(new Response<>(HttpStatus.OK.value(), HttpStatus.OK.name(),
            new ArrayList<>((Collections.singletonList(new GetSellerLogisticProductResponse()))),
            new HashMap<>(), new HashMap<>()));

    when(sellerLogisticsFeign.getTemplateData(BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(new Response<>(HttpStatus.OK.value(), HttpStatus.OK.name(),
            new DownloadSkuTemplateResponse(), new HashMap<>(), new HashMap<>()));


    when(sellerLogisticsFeign.excelUploadStatus(BUSINESS_PARTNER_CODE))
        .thenReturn(new Response<>(HttpStatus.OK.value(), HttpStatus.OK.name(),
            new UploadExcelSkuUpdateStatusResponse(), new HashMap<>(), new HashMap<>()));
  }

  @Test
  public void getSellerLogisticsProductTest() throws Exception {
    sellerLogisticsService.getSellerLogisticsProduct(BUSINESS_PARTNER_CODE, PICKUP);
    verify(sellerLogisticsFeign).getSellerLogisticProducts(BUSINESS_PARTNER_CODE, PICKUP);
  }

  @Test
  public void getSellerLogisticsProductTest_nullResponse() throws Exception {
    when(sellerLogisticsFeign.getSellerLogisticProducts(BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ClientException.class,
          () -> sellerLogisticsService.getSellerLogisticsProduct(BUSINESS_PARTNER_CODE, PICKUP));
    } finally {
      verify(sellerLogisticsFeign).getSellerLogisticProducts(BUSINESS_PARTNER_CODE, PICKUP);
    }
  }

  @Test
  public void getTemplateDataTest() throws Exception {
    sellerLogisticsService.getTemplateData(BUSINESS_PARTNER_CODE, PICKUP);
    verify(sellerLogisticsFeign).getTemplateData(BUSINESS_PARTNER_CODE, PICKUP);
  }

  @Test
  public void getTemplateDataTest_StatusFailed() {
    when(sellerLogisticsFeign.getTemplateData(BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(),
            new DownloadSkuTemplateResponse(), errorMap, new HashMap<>()));

    try {
      Assertions.assertThrows(ClientException.class,
          () -> sellerLogisticsService.getTemplateData(BUSINESS_PARTNER_CODE, PICKUP));
    } finally {
      verify(sellerLogisticsFeign).getTemplateData(BUSINESS_PARTNER_CODE, PICKUP);
    }
  }

  @Test
  public void getTemplateDataTest_StatusFailed_nullErrorMap() throws Exception {
    when(sellerLogisticsFeign.getTemplateData(BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(),
            new DownloadSkuTemplateResponse(), null, new HashMap<>()));

    try {
      Assertions.assertThrows(ClientException.class,
          () -> sellerLogisticsService.getTemplateData(BUSINESS_PARTNER_CODE, PICKUP));
    } finally {
      verify(sellerLogisticsFeign).getTemplateData(BUSINESS_PARTNER_CODE, PICKUP);
    }
  }

  @Test
  public void getTemplateDataTest_StatusFailed_emptyErrorMap() throws Exception {
    Map<String, List<String>> error = new HashMap<>();
    error.put("ERROR", new ArrayList<>());
    when(sellerLogisticsFeign.getTemplateData(BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(),
            new DownloadSkuTemplateResponse(), error, new HashMap<>()));

    try {
      Assertions.assertThrows(ClientException.class,
          () -> sellerLogisticsService.getTemplateData(BUSINESS_PARTNER_CODE, PICKUP));
    } finally {
      verify(sellerLogisticsFeign).getTemplateData(BUSINESS_PARTNER_CODE, PICKUP);
    }
  }

  @Test
  public void getTemplateDataTest_StatusFailed_emptyError() throws Exception {
    Map<String, List<String>> error = new HashMap<>();
    error.put("ERROR", Arrays.asList(""));
    when(sellerLogisticsFeign.getTemplateData(BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(),
            new DownloadSkuTemplateResponse(), error, new HashMap<>()));

    try {
      Assertions.assertThrows(ClientException.class,
          () -> sellerLogisticsService.getTemplateData(BUSINESS_PARTNER_CODE, PICKUP));
    } finally {
      verify(sellerLogisticsFeign).getTemplateData(BUSINESS_PARTNER_CODE, PICKUP);
    }
  }

  @Test
  public void excelUploadTest() throws Exception {
    MultipartFile file = generateDummyExcelMultipartFile();
    UploadExcelRequest uploadExcelRequest = new UploadExcelRequest();
    uploadExcelRequest.setEncodedContent(Base64.encodeBase64String(file.getBytes()));
    uploadExcelRequest.setFileName(file.getOriginalFilename());
    when(sellerLogisticsFeign.excelUpload(uploadExcelRequest, BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(new Response<>(HttpStatus.OK.value(), HttpStatus.OK.name(),
            new UploadExcelSkuUpdateResponse(), new HashMap<>(), new HashMap<>()));
    sellerLogisticsService.excelUpload(file, BUSINESS_PARTNER_CODE, PICKUP);
    verify(sellerLogisticsFeign).excelUpload(uploadExcelRequest, BUSINESS_PARTNER_CODE, PICKUP);
  }

  @Test
  public void getExcelUploadStatusTest() throws Exception {
    sellerLogisticsService.excelUploadStatus(BUSINESS_PARTNER_CODE);
    verify(sellerLogisticsFeign).excelUploadStatus(BUSINESS_PARTNER_CODE);
  }

  @Test
  public void getExcelUploadStatusTest_dataNull() throws Exception {
    when(sellerLogisticsFeign.excelUploadStatus(BUSINESS_PARTNER_CODE)).thenReturn(new Response<>(
        HttpStatus.OK.value(), HttpStatus.OK.name(), null, new HashMap<>(), new HashMap<>()));
    try {
      Assertions.assertThrows(ClientException.class,
          () -> sellerLogisticsService.excelUploadStatus(BUSINESS_PARTNER_CODE));
    } finally {
      verify(sellerLogisticsFeign).excelUploadStatus(BUSINESS_PARTNER_CODE);
    }
  }

  private MultipartFile generateDummyExcelMultipartFile() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile =
        new MockMultipartFile("dummy-excel", DUMMY_FILE_NAME, null, fileData);
    return multipartFile;
  }

  private File generateDummyExcelFile() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file =
        new File(classLoader.getResource(FILE_FOLDER + File.separator + DUMMY_FILE_NAME).getFile());
    return file;
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(sellerLogisticsFeign);
  }
}
