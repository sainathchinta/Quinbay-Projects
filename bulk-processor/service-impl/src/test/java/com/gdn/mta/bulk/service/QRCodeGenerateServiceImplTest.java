package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.AllowedQRGenerationType;

import com.gdn.mta.bulk.config.QRCodeProperties;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.QrCodeRowInfo;
import com.gdn.mta.bulk.dto.QrCodeRowItemInfo;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.x.product.rest.web.model.response.PriceRangeResponse;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.core.io.FileSystemResource;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.ResourceUtils;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public class QRCodeGenerateServiceImplTest {

  @InjectMocks
  private QRCodeGenerateServiceImpl qrCodeGenerateService;
  @Mock
  private BulkProcessDataService bulkProcessDataService;
  @Mock
  private ObjectMapper objectMapper;
  @Mock
  private HtmlTemplateParserService velocityTemplateParserService;
  @Mock
  private FileStorageService fileStorageService;
  @Mock
  private QRCodeProperties qrCodeProperties;
  @Mock
  private XProductOutboundService xProductOutboundService;
  @Mock
  private CMSOutboundService cmsOutboundService;
  private static final String PRODUCT_URL = "https://wwwuatb.gdn-app.com/p/p/productSku?campaign_source=merchant_product_scan";
  private static final String MERCHANT_URL = "https://wwwuatb.gdn-app.com/merchant/m/merchantCode?campaign_source=merchant_product_scan";
  private static final String ADD_TO_BAG_URL =
      "https://wwwuatb.gdn-app.com/qr/scan?action=addToCart&next=cartPage&cartType=SCANandGO"
          + "&instantPickup=true&quantity=1";

  private BulkUpdateEventModel bulkUpdateEventModel;
  private BulkProcessData bulkProcessData;
  private static final int ROW_NUMBER = 4;
  private static final String STORE_ID = "10001";
  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private static final String BULK_PROCESS_CODE = "BULK_PROCESS_CODE";
  private static final String SHORT_URL = "shortUrl.com";
  private static final String URL_SHORTENER_SWITCH_MAP = "{\"ADD_TO_BAG\": true}";

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    bulkUpdateEventModel =
        new BulkUpdateEventModel(STORE_ID, BUSINESS_PARTNER_CODE, BULK_PROCESS_CODE,
            Collections.singletonList(ROW_NUMBER));
    bulkProcessData = new BulkProcessData();
    bulkProcessData.setBulkRequestData(StringUtils.EMPTY);
    ReflectionTestUtils.setField(qrCodeGenerateService, "qrcodeColorRgb", "255,255,255");
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(bulkProcessDataService);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(velocityTemplateParserService);
    verifyNoMoreInteractions(fileStorageService);
    verifyNoMoreInteractions(cmsOutboundService);
  }

  @Test
  public void generateQRCodeTestSuccess() throws Exception {
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(1);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.PRODUCT.name());
    qrCodeRowInfo.setTemplateSize("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(new QrCodeRowItemInfo()));
    when(qrCodeProperties.getBlibliPinBig()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PRODUCT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlProduct();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }

  @Test
  public void generateQRCodeTestSuccessTwoPage() throws Exception {
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(2);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ITEM.name());
    qrCodeRowInfo.setTemplateSize("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(new QrCodeRowItemInfo()));
    when(qrCodeProperties.getBlibliPinBig()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PRODUCT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlProduct();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }

  @Test
  public void generateQRCodeTestSuccessL5() throws Exception {
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(16);
    qrCodeRowInfo.setPrintPrice(true);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ITEM_PICKUP_POINT.name());
    qrCodeRowInfo.setTemplateSize("BY39");
    QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setPickupPointCode("test");
    qrCodeRowItemInfo.setCncActivated(true);
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PRODUCT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlProduct();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }

  @Test
  public void generateQRCodeTestSuccessL5_shortenedUrl() throws Exception {
    ReflectionTestUtils.setField(qrCodeGenerateService, "urlShortenerSwitchMap",
        Collections.singletonMap(AllowedQRGenerationType.ITEM_PICKUP_POINT.getValue(), true));
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(16);
    qrCodeRowInfo.setPrintPrice(true);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ITEM_PICKUP_POINT.name());
    qrCodeRowInfo.setTemplateSize("BY919");
    QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setPickupPointCode("test");
    qrCodeRowItemInfo.setCncActivated(true);
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PRODUCT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    when(cmsOutboundService.mapLongAndShortUrl(eq(SHORT_URL), Mockito.anyString())).thenReturn(true);
    when(cmsOutboundService.generateShortUrl()).thenReturn(SHORT_URL);
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlProduct();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
    verify(cmsOutboundService).mapLongAndShortUrl(eq(SHORT_URL), Mockito.anyString());
    verify(cmsOutboundService).generateShortUrl();
  }

  @Test
  public void generateQRCodeTestSuccessL5EmptyPickUpPoint() throws Exception {
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(32);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ITEM_PICKUP_POINT.name());
    qrCodeRowInfo.setTemplateSize("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(new QrCodeRowItemInfo()));
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PRODUCT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlProduct();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }

  @Test
  public void generateQRCodeTestSuccessStore() throws Exception {
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(16);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.STORE.name());
    qrCodeRowInfo.setTemplateSize("test");
    QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setPickupPointCode("test");
    qrCodeRowInfo.setMerchantCode("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlMerchant()).thenReturn(MERCHANT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlMerchant();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }

  @Test
  public void generateQRCodeTestSuccessStore_shortenedUrl() throws Exception {
    ReflectionTestUtils.setField(qrCodeGenerateService, "urlShortenerSwitchMap",
        Collections.singletonMap(AllowedQRGenerationType.STORE.getValue(), true));
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(16);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.STORE.name());
    qrCodeRowInfo.setTemplateSize("test");
    QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setPickupPointCode("test");
    qrCodeRowInfo.setMerchantCode("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlMerchant()).thenReturn(MERCHANT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    when(cmsOutboundService.mapLongAndShortUrl(eq(SHORT_URL), Mockito.anyString())).thenReturn(true);
    when(cmsOutboundService.generateShortUrl()).thenReturn(SHORT_URL);
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlMerchant();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
    verify(cmsOutboundService).mapLongAndShortUrl(eq(SHORT_URL), Mockito.anyString());
    verify(cmsOutboundService).generateShortUrl();
  }

  @Test
  public void generateQRCodeTestSuccessStoreEmptyPickUpPoint() throws Exception {
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(32);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.STORE.name());
    qrCodeRowInfo.setTemplateSize("test");
    qrCodeRowInfo.setMerchantCode("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(new QrCodeRowItemInfo()));
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlMerchant()).thenReturn(MERCHANT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlMerchant();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }


  @Test
  public void generateQRCodeTestSuccessL3WithPrice() throws Exception {
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(16);
    qrCodeRowInfo.setPrintPrice(true);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.PRODUCT.name());
    qrCodeRowInfo.setTemplateSize("test");
    QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setProductSku("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    List<PriceRangeResponse> rangeResponses = new ArrayList<>();
    rangeResponses.add(new PriceRangeResponse("test", "1234", "1234"));
    rangeResponses.add(new PriceRangeResponse("test", "1234", "1234"));
    when(xProductOutboundService.fetchPriceRange(eq(BUSINESS_PARTNER_CODE), any())).thenReturn(rangeResponses);
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PRODUCT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlProduct();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(xProductOutboundService).fetchPriceRange(eq(BUSINESS_PARTNER_CODE), any());
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }

  @Test
  public void generateQRCodeTestSuccessL3WithExcel() throws Exception {
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(16);
    qrCodeRowInfo.setPrintPrice(false);
    qrCodeRowInfo.setBulkExcelUpload(true);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.PRODUCT.name());
    qrCodeRowInfo.setTemplateSize("test");
    QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setProductSku("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    List<PriceRangeResponse> rangeResponses = new ArrayList<>();
    rangeResponses.add(new PriceRangeResponse("test", "1234", "1234"));
    rangeResponses.add(new PriceRangeResponse("test", "1234", "1234"));
    when(xProductOutboundService.fetchPriceRange(eq(BUSINESS_PARTNER_CODE), any())).thenReturn(rangeResponses);
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PRODUCT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlProduct();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(xProductOutboundService).fetchPriceRange(eq(BUSINESS_PARTNER_CODE), any());
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }

  @Test
  public void generateQRCodeTestSuccessL3WithPriceEmpty() throws Exception {
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(16);
    qrCodeRowInfo.setPrintPrice(true);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.PRODUCT.name());
    qrCodeRowInfo.setTemplateSize("test");
    QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setProductSku("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    List<PriceRangeResponse> rangeResponses = new ArrayList<>();
    rangeResponses.add(new PriceRangeResponse("test", "", ""));
    when(xProductOutboundService.fetchPriceRange(eq(BUSINESS_PARTNER_CODE), any())).thenReturn(rangeResponses);
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PRODUCT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlProduct();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(xProductOutboundService).fetchPriceRange(eq(BUSINESS_PARTNER_CODE), any());
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }

  @Test
  public void generateQRCodeTestSuccessL3WithPriceExists() throws Exception {
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(16);
    qrCodeRowInfo.setPrintPrice(true);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.PRODUCT.name());
    qrCodeRowInfo.setTemplateSize("test");
    QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setProductSku("test");
    qrCodeRowItemInfo.setProductPrice("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    List<PriceRangeResponse> rangeResponses = new ArrayList<>();
    rangeResponses.add(new PriceRangeResponse("test", "1234", "1234"));
    rangeResponses.add(new PriceRangeResponse("test", "1234", "1234"));
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PRODUCT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlProduct();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }

  @Test
  public void generateQRCodeTestSuccessL4WithPrice() throws Exception {
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(16);
    qrCodeRowInfo.setPrintPrice(true);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ITEM.name());
    qrCodeRowInfo.setTemplateSize("test");
    QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setItemSku("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    List<PriceRangeResponse> rangeResponses = new ArrayList<>();
    rangeResponses.add(new PriceRangeResponse("test1", "1234", "1234"));
    when(xProductOutboundService.fetchPriceRange(eq(BUSINESS_PARTNER_CODE), any())).thenReturn(rangeResponses);
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlProduct()).thenReturn(PRODUCT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlProduct();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(xProductOutboundService).fetchPriceRange(eq(BUSINESS_PARTNER_CODE), any());
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }

  @Test
  public void generateQRCodeTestError() throws Exception {
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    directory.mkdir();
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(1);
    qrCodeRowInfo.setQrGenerationType("test");
    qrCodeRowInfo.setTemplateSize("test");
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenThrow(
        new RuntimeException());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(bulkProcessDataService).saveOperation(any());
  }

  @Test
  public void generateQRCodeTestSuccessAddToBag() throws Exception {
    ReflectionTestUtils.setField(qrCodeGenerateService, "urlShortenerSwitchMap",
        Collections.singletonMap(AllowedQRGenerationType.ADD_TO_BAG.getValue(), false));
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(16);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.name());
    qrCodeRowInfo.setTemplateSize("test");
    QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setPickupPointCode("test");
    qrCodeRowInfo.setMerchantCode("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlAddToBag()).thenReturn(ADD_TO_BAG_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlAddToBag();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }

  @Test
  public void generateQRCodeTestSuccessAddToBag_shortenedUrl() throws Exception {
    ReflectionTestUtils.setField(qrCodeGenerateService, "urlShortenerSwitchMap",
        Collections.singletonMap(AllowedQRGenerationType.ADD_TO_BAG.getValue(), true));
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(16);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.name());
    qrCodeRowInfo.setTemplateSize("test");
    QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setPickupPointCode("test");
    qrCodeRowInfo.setMerchantCode("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlAddToBag()).thenReturn(ADD_TO_BAG_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    when(cmsOutboundService.mapLongAndShortUrl(eq(SHORT_URL), Mockito.anyString())).thenReturn(true);
    when(cmsOutboundService.generateShortUrl()).thenReturn(SHORT_URL);
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlAddToBag();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
    verify(cmsOutboundService).generateShortUrl();
    verify(cmsOutboundService).mapLongAndShortUrl(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void generateQRCodeTestSuccessAddToBag_shortenedUrlThrowsError() throws Exception {
    ReflectionTestUtils.setField(qrCodeGenerateService, "urlShortenerSwitchMap",
        Collections.singletonMap(AllowedQRGenerationType.ADD_TO_BAG.getValue(), true));
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(16);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ADD_TO_BAG.name());
    qrCodeRowInfo.setTemplateSize("test");
    QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setPickupPointCode("test");
    qrCodeRowInfo.setMerchantCode("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlAddToBag()).thenReturn(ADD_TO_BAG_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    when(cmsOutboundService.mapLongAndShortUrl(eq(SHORT_URL), Mockito.anyString())).thenThrow(
        new ApplicationRuntimeException());
    when(cmsOutboundService.generateShortUrl()).thenReturn(SHORT_URL);
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlAddToBag();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
    verify(cmsOutboundService).generateShortUrl();
    verify(cmsOutboundService).mapLongAndShortUrl(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void generateQRCodeTestSuccessAllProducts() throws Exception {
    File file = ResourceUtils.getFile("classpath:blibliPinSmall.png");
    File directory = new File(new FileSystemResource("").getFile().getAbsolutePath(),
        File.separator + QRCodeGenerateServiceImpl.DEFAULT_FOLDER);
    FileUtils.deleteDirectory(directory);
    QrCodeRowInfo qrCodeRowInfo = new QrCodeRowInfo();
    qrCodeRowInfo.setQrPerPage(16);
    qrCodeRowInfo.setQrGenerationType(AllowedQRGenerationType.ALL_PRODUCTS.name());
    qrCodeRowInfo.setTemplateSize("test");
    QrCodeRowItemInfo qrCodeRowItemInfo = new QrCodeRowItemInfo();
    qrCodeRowItemInfo.setPickupPointCode("SEMUA LOKASI");
    qrCodeRowInfo.setMerchantCode("test");
    qrCodeRowInfo.setRowItems(Collections.singletonList(qrCodeRowItemInfo));
    when(qrCodeProperties.getBlibliPinSmall()).thenReturn(StringUtils.EMPTY);
    when(qrCodeProperties.getPrefixUrlMerchant()).thenReturn(MERCHANT_URL);
    when(fileStorageService.downloadFileFromGcs(StringUtils.EMPTY)).thenReturn(
        Files.readAllBytes(file.toPath()));
    when(bulkProcessDataService.findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING)).thenReturn(
        Collections.singletonList(bulkProcessData));
    when(fileStorageService.fetchQRCodeTemplate(qrCodeRowInfo)).thenReturn(
        StringUtils.EMPTY);
    when(objectMapper.readValue(StringUtils.EMPTY, QrCodeRowInfo.class)).thenReturn(qrCodeRowInfo);
    Map<String, Object> data = new HashMap();
    when(objectMapper.convertValue(qrCodeRowInfo, Map.class)).thenReturn(data);
    when(velocityTemplateParserService.parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data)).thenReturn(
        StringUtils.EMPTY);
    when(fileStorageService.uploadFileToGcs(anyString(), anyString(), any())).thenReturn(true);
    when(bulkProcessDataService.saveOperation(any())).thenReturn(new BulkProcessData());
    qrCodeGenerateService.generateQRCode(bulkUpdateEventModel);
    verify(bulkProcessDataService).findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(
        bulkUpdateEventModel.getStoreId(), bulkUpdateEventModel.getBulkProcessCode(),
        bulkUpdateEventModel.getRowNumbers(), BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(StringUtils.EMPTY, QrCodeRowInfo.class);
    verify(fileStorageService).fetchQRCodeTemplate(qrCodeRowInfo);
    verify(fileStorageService).uploadFileToGcs(anyString(), anyString(), any());
    verify(bulkProcessDataService).saveOperation(any());
    verify(objectMapper).convertValue(qrCodeRowInfo, Map.class);
    verify(velocityTemplateParserService).parseVelocityTemplateToHtml(
        bulkUpdateEventModel.getBulkProcessCode(), StringUtils.EMPTY, data);
    verify(qrCodeProperties).getPrefixUrlMerchant();
    verify(qrCodeProperties).getBlibliPinSmall();
    verify(fileStorageService).downloadFileFromGcs(StringUtils.EMPTY);
    verify(fileStorageService).getBasePath(BulkProcessType.QR_GENERATION.getValue());
  }
}
