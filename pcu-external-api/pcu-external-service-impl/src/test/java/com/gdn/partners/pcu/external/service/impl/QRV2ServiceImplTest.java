package com.gdn.partners.pcu.external.service.impl;


import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.mta.bulk.dto.QrExcelUploadRequest;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.partners.pcu.external.client.feign.XBulkFeign;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.external.service.impl.config.KafkaTopicProperties;
import com.gdn.partners.pcu.external.web.model.enums.TemplateSize;
import com.gdn.partners.pcu.external.web.model.request.ManualQRCodeRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductDetailsRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.partners.pcu.external.service.impl.helper.BeanUtils;
import org.springframework.mock.web.MockMultipartFile;

import java.util.ArrayList;
import java.util.List;

import static com.gdn.partners.pcu.external.model.Constants.BUSINESS_PARTNER_NAME;
import static com.gdn.partners.pcu.external.model.Constants.REQUEST_ID;
import static com.gdn.partners.pcu.external.model.Constants.STORE_ID;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public class QRV2ServiceImplTest {

  private static final String PRODUCT_SKU = "product-sku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String ITEM_SKU = "itemSku";
  private static final String UNAUTHORISED_ERROR = "Unauthorize access :You are not authorized";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String GENERATE_QR_CODE_PRODUCT = "GENERATE_QR_CODE_PRODUCT";
  private static final String GENERATE_QR_CODE_STORE = "GENERATE_QR_CODE_STORE";

  private ManualQRCodeRequest manualQRCodeRequest;
  private ManualQRCodeRequest manualQRCodeRequestForProduct;
  private ProductDetailsRequest productDetailsRequest;
  private MockMultipartFile multipartFile =
      new MockMultipartFile("dummy.xlsx", "dummy.xlsx", null, new byte[10]);

  @Mock
  private XBulkFeign xBulkFeign;

  @Mock
  private KafkaPublisher kafkaProducer;

  @InjectMocks
  private QRV2ServiceImpl qrv2Service;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    productDetailsRequest = new ProductDetailsRequest();
    productDetailsRequest.setProductSku(PRODUCT_SKU);
    productDetailsRequest.setPickupPointCode(PICKUP_POINT_CODE);
    productDetailsRequest.setItemSku(ITEM_SKU);

    manualQRCodeRequest = new ManualQRCodeRequest();
    manualQRCodeRequest.setAllStores(false);
    manualQRCodeRequest.setIsDarkTheme(true);
    manualQRCodeRequest.setQrPerPage(1);
    manualQRCodeRequest.setTemplateSize(TemplateSize.A5);
    manualQRCodeRequest.setQrGenerationType(AllowedQRGenerationType.STORE);
    List<ProductDetailsRequest> requests = new ArrayList<>();
    requests.add(productDetailsRequest);
    manualQRCodeRequest.setProductDetailsRequestList(requests);
    manualQRCodeRequest.setCncActivated(false);

    manualQRCodeRequestForProduct = new ManualQRCodeRequest();
    manualQRCodeRequestForProduct.setAllStores(false);
    manualQRCodeRequestForProduct.setIsDarkTheme(true);
    manualQRCodeRequestForProduct.setQrPerPage(1);
    manualQRCodeRequestForProduct.setTemplateSize(TemplateSize.A5);
    manualQRCodeRequestForProduct.setQrGenerationType(AllowedQRGenerationType.ITEM);
    manualQRCodeRequestForProduct.setProductDetailsRequestList(requests);
    manualQRCodeRequestForProduct.setCncActivated(true);
  }

  @Test
  public void qrGenerationAccessibleSuccess() {
    when(xBulkFeign.checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnBaseRestResponse(Boolean.FALSE));
    boolean result = qrv2Service.qrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(result);
    verify(xBulkFeign).checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE);
  }

  @Test
  public void downloadQRCodesTest() throws Exception {
    DownloadQRCodeRequest downloadQRCodeRequest = new DownloadQRCodeRequest();
    BeanUtils.copyProperties(manualQRCodeRequest, downloadQRCodeRequest);
    downloadQRCodeRequest.setStoreId(STORE_ID);
    downloadQRCodeRequest.setRequestId(REQUEST_ID);
    downloadQRCodeRequest.setMerchantCode(Constants.BUSINESS_PARTNER_CODE);
    downloadQRCodeRequest.setTemplateSize(manualQRCodeRequest.getTemplateSize().name());
    downloadQRCodeRequest.setQrGenerationType(manualQRCodeRequest.getQrGenerationType().name());
    downloadQRCodeRequest.setMerchantName(BUSINESS_PARTNER_NAME);
    when(xBulkFeign.checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnBaseRestResponse(Boolean.TRUE));
    when(kafkaTopicProperties.getGenerateQrCodeStore()).thenReturn(GENERATE_QR_CODE_STORE);
    this.qrv2Service.downloadQRCodes(STORE_ID, REQUEST_ID, Constants.BUSINESS_PARTNER_CODE,
        BUSINESS_PARTNER_NAME, manualQRCodeRequest);
    verify(kafkaProducer).send(GENERATE_QR_CODE_STORE, downloadQRCodeRequest);
    verify(xBulkFeign).checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE);
    verify(kafkaTopicProperties).getGenerateQrCodeStore();
  }

  @Test
  public void downloadQrCodesTest_exception() {
    when(xBulkFeign.checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnBaseRestResponse(Boolean.FALSE));

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.qrv2Service.downloadQRCodes(STORE_ID, REQUEST_ID,
              Constants.BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, manualQRCodeRequest));
    } finally {
      verify(xBulkFeign).checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void downloadQRCodesTestForProduct() throws Exception {
    DownloadQRCodeRequest downloadQRCodeRequest = new DownloadQRCodeRequest();
    BeanUtils.copyProperties(manualQRCodeRequest, downloadQRCodeRequest);
    downloadQRCodeRequest.setStoreId(STORE_ID);
    downloadQRCodeRequest.setRequestId(REQUEST_ID);
    downloadQRCodeRequest.setMerchantCode(Constants.BUSINESS_PARTNER_CODE);
    downloadQRCodeRequest.setTemplateSize(manualQRCodeRequest.getTemplateSize().name());
    downloadQRCodeRequest.setQrGenerationType(manualQRCodeRequest.getQrGenerationType().name());
    downloadQRCodeRequest.setMerchantName(BUSINESS_PARTNER_NAME);
    downloadQRCodeRequest.setQrGenerationType(AllowedQRGenerationType.ITEM.name());
    downloadQRCodeRequest.setCncActivated(true);
    when(xBulkFeign.checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnBaseRestResponse(Boolean.TRUE));
    when(kafkaTopicProperties.getGenerateQrCodeProduct()).thenReturn(GENERATE_QR_CODE_PRODUCT);
    this.qrv2Service.downloadQRCodes(STORE_ID, REQUEST_ID, Constants.BUSINESS_PARTNER_CODE,
        BUSINESS_PARTNER_NAME, manualQRCodeRequestForProduct);
    verify(kafkaProducer).send(GENERATE_QR_CODE_PRODUCT, downloadQRCodeRequest);
    verify(xBulkFeign).checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE);
    verify(kafkaTopicProperties).getGenerateQrCodeProduct();
  }

  @Test
  public void uploadExcelAndPublishRequest_xlsxTest() throws Exception {
    manualQRCodeRequest.setQrGenerationType(AllowedQRGenerationType.PRODUCT);
    when(this.xBulkFeign.uploadQrCodeExcel(eq(STORE_ID), eq(REQUEST_ID),
        Mockito.any(QrExcelUploadRequest.class))).thenReturn(new GdnBaseRestResponse(true));
    when(xBulkFeign.checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnBaseRestResponse(Boolean.TRUE));
    this.qrv2Service.uploadExcelAndPublishRequest(STORE_ID, REQUEST_ID,
        Constants.BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, manualQRCodeRequest, multipartFile);
    verify(this.xBulkFeign).checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE);
    verify(this.xBulkFeign).uploadQrCodeExcel(eq(STORE_ID), eq(REQUEST_ID),
        Mockito.any(QrExcelUploadRequest.class));
  }

  @Test
  public void uploadExcelAndPublishRequest_xlsTest() throws Exception {
    multipartFile = new MockMultipartFile("supported.xls", "supported.xls", null, new byte[10]);
    manualQRCodeRequest.setQrGenerationType(AllowedQRGenerationType.PRODUCT);
    when(this.xBulkFeign.uploadQrCodeExcel(eq(STORE_ID), eq(REQUEST_ID),
        Mockito.any(QrExcelUploadRequest.class))).thenReturn(new GdnBaseRestResponse(true));
    when(xBulkFeign.checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnBaseRestResponse(Boolean.TRUE));
    this.qrv2Service.uploadExcelAndPublishRequest(STORE_ID, REQUEST_ID,
        Constants.BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, manualQRCodeRequest, multipartFile);
    verify(this.xBulkFeign).checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE);
    verify(this.xBulkFeign).uploadQrCodeExcel(eq(STORE_ID), eq(REQUEST_ID),
        Mockito.any(QrExcelUploadRequest.class));
  }

  @Test
  public void uploadExcelAndPublishRequest_invalidFileTypeTest() throws Exception {
    multipartFile = new MockMultipartFile("not-supported.txt", "not-supported.txt", null, new byte[10]);
    when(xBulkFeign.checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnBaseRestResponse(Boolean.FALSE));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.qrv2Service.uploadExcelAndPublishRequest(STORE_ID, REQUEST_ID,
              Constants.BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, manualQRCodeRequest,
              multipartFile));
    } finally {
      verifyNoInteractions(this.xBulkFeign);
    }
  }

  @Test
  public void uploadExcelAndPublishRequest_exceptionTest() throws Exception {
    when(xBulkFeign.checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE)).thenReturn(
      new GdnBaseRestResponse(Boolean.FALSE));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.qrv2Service.uploadExcelAndPublishRequest(STORE_ID, REQUEST_ID,
              Constants.BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, manualQRCodeRequest,
              multipartFile));
    } finally {
      verify(this.xBulkFeign).checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void downloadQRCodesTest_allProducts() throws Exception {
    manualQRCodeRequest.setQrGenerationType(AllowedQRGenerationType.ALL_PRODUCTS);
    DownloadQRCodeRequest downloadQRCodeRequest = new DownloadQRCodeRequest();
    BeanUtils.copyProperties(manualQRCodeRequest, downloadQRCodeRequest);
    downloadQRCodeRequest.setStoreId(STORE_ID);
    downloadQRCodeRequest.setRequestId(REQUEST_ID);
    downloadQRCodeRequest.setMerchantCode(Constants.BUSINESS_PARTNER_CODE);
    downloadQRCodeRequest.setTemplateSize(manualQRCodeRequest.getTemplateSize().name());
    downloadQRCodeRequest.setQrGenerationType(manualQRCodeRequest.getQrGenerationType().name());
    downloadQRCodeRequest.setMerchantName(BUSINESS_PARTNER_NAME);
    when(xBulkFeign.checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnBaseRestResponse(Boolean.TRUE));
    when(kafkaTopicProperties.getGenerateQrCodeStore()).thenReturn(GENERATE_QR_CODE_STORE);
    this.qrv2Service.downloadQRCodes(STORE_ID, REQUEST_ID, Constants.BUSINESS_PARTNER_CODE,
        BUSINESS_PARTNER_NAME, manualQRCodeRequest);
    verify(kafkaProducer).send(GENERATE_QR_CODE_STORE, downloadQRCodeRequest);
    verify(xBulkFeign).checkQrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE);
    verify(kafkaTopicProperties).getGenerateQrCodeStore();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(xBulkFeign);
    verifyNoMoreInteractions(kafkaProducer);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }
}
