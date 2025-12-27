package com.gdn.mta.bulk.helper;

import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.OrderDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkOrderResponse;
import com.gdn.mta.bulk.models.download.responsedata.OrderItemSummaryResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.neo.order.client.sdk.web.model.request.OrderItemSummaryRequest;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.CollectionUtils;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;


import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

/**
 * Created by keshashah on 16/11/16.
 */
public class BulkOrderProcessHelperTest {

  private static final String DEFAULT_REQUEST_ID = "REQUEST ID";
  private static final String OFFLINE_ORDER_TYPE = "OFF2ON";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String ORDER_HANDED_OVER = "Pesanan Diserahkan";
  private static final String PAYMENT_DISPLAY_NAME = "Metode pembayaran";
  private static final String PACKAGE_ID = "No. Paket";
  private static final String COMMISSION = "Komisi";
  private static final String SALESMAN_EMAIL = "E-mail Sales pendamping";

  private OrderItemSummaryRequest orderItemSummaryRequest;
  private OrderItemSummaryRequest orderItemSummaryRequestOff2On;

  @InjectMocks
  private BulkOrderProcessHelper bulkOrderProcessHelper;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @BeforeEach
  public void setUp() throws Exception {
      initMocks(this);

      orderItemSummaryRequest = new OrderItemSummaryRequest();
      List<String> orderType = new ArrayList<>(Collections.singletonList(""));
      orderItemSummaryRequest.setOrderType(orderType);

      orderItemSummaryRequestOff2On = new OrderItemSummaryRequest();
      List<String> orderTypeOffline = new ArrayList<>(Collections.singletonList(OFFLINE_ORDER_TYPE));
      orderItemSummaryRequestOff2On.setOrderType(orderTypeOffline);

    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            "true", ""));
  }

  @Test
  public void getCsvHeadersMap_returnSolrHeaders() throws Exception {
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest);
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(builder.build());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED);
    Assertions.assertNotNull(csvModel);
    Assertions.assertFalse(CollectionUtils.isEmpty(csvModel.getHeaderList()));
    Assertions.assertFalse(csvModel.getHeaderToFieldMap().isEmpty());
    Assertions.assertTrue(csvModel.getHeaderList().size() == csvModel.getHeaderToFieldMap().keySet().size());
  }

  @Test
  public void getCsvHeadersMap_returnDbHeaders() throws Exception {
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            "false", ""));
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(builder.build());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED);
    Assertions.assertNotNull(csvModel);
    Assertions.assertFalse(CollectionUtils.isEmpty(csvModel.getHeaderList()));
    Assertions.assertFalse(csvModel.getHeaderToFieldMap().isEmpty());
    Assertions.assertTrue(csvModel.getHeaderList().size() == csvModel.getHeaderToFieldMap().keySet().size());
    Assertions.assertEquals(PACKAGE_ID,csvModel.getHeaderList().get(2));
    Assertions.assertEquals(COMMISSION,csvModel.getHeaderList().get(26));
  }

  @Test
  public void getCsvHeadersMap_returnDBHeaders() throws Exception {
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED,
            "false", ""));
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest);
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(builder.build());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED);
    Assertions.assertNotNull(csvModel);
    Assertions.assertFalse(CollectionUtils.isEmpty(csvModel.getHeaderList()));
    Assertions.assertFalse(csvModel.getHeaderToFieldMap().isEmpty());
    Assertions.assertTrue(csvModel.getHeaderList().size() == csvModel.getHeaderToFieldMap().keySet().size());
  }

  @Test
  public void getCsvHeadersMapReturnOff2OnHeaders() throws Exception{
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequestOff2On);
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(builder.build());
  }

  @Test
  public void getCsvHeadersMapReturnOfflineHeaders() throws Exception{
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest).offline(true);
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(builder.build());
  }

  @Test
  public void getCsvHeadersMapReturnOffline_switchOnNonFbbSellerHeaders() throws Exception{
    ReflectionTestUtils.setField(bulkOrderProcessHelper, "handoverFeatureSwitch", true);
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest).offline(true);
    OrderDownloadRequest orderDownloadRequest = builder.build();
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId())))
        .thenReturn(new ProfileResponse());
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(orderDownloadRequest);
    Assertions.assertFalse(csvModel.getHeaderList().contains(ORDER_HANDED_OVER));
    Mockito.verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId()));
  }

  @Test
  public void getCsvHeadersMapReturnOffline_switchOnFbbSellerHeaders() throws Exception {
    ReflectionTestUtils.setField(bulkOrderProcessHelper, "handoverFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setFbbActivated(true);
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest).offline(true);
    OrderDownloadRequest orderDownloadRequest = builder.build();
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId())))
        .thenReturn(profileResponse);
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(orderDownloadRequest);
    Mockito.verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId()));
    Assertions.assertFalse(csvModel.getHeaderList().contains(ORDER_HANDED_OVER));
  }

  @Test
  public void getCsvHeadersMapReturnOffline_switchOnFbbSellerHeadersPaymentMethod() throws Exception {
    ReflectionTestUtils.setField(bulkOrderProcessHelper, "handoverFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setFbbActivated(true);
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(true);
    profileResponse.setCompany(company);
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest).offline(true);
    builder.orderRequest(orderItemSummaryRequest).showPaymentMethodColumn(true);
    OrderDownloadRequest orderDownloadRequest = builder.build();
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId())))
        .thenReturn(profileResponse);
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(orderDownloadRequest);
    Mockito.verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId()));
    Assertions.assertTrue(csvModel.getHeaderList().contains(PAYMENT_DISPLAY_NAME));
  }

  @Test
  public void getCsvHeadersMapReturnOffline_switchOnFbbAndCncSellerHeaders() throws Exception {
    ReflectionTestUtils.setField(bulkOrderProcessHelper, "handoverFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setFbbActivated(true);
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(true);
    profileResponse.setCompany(company);
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest).offline(true);
    OrderDownloadRequest orderDownloadRequest = builder.build();
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId())))
        .thenReturn(profileResponse);
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(orderDownloadRequest);
    Mockito.verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId()));
    Assertions.assertTrue(csvModel.getHeaderList().contains(ORDER_HANDED_OVER));
  }

  @Test
  public void getCsvHeadersMapReturnOffline_switchOnXBPFeignError() throws Exception {
    ReflectionTestUtils.setField(bulkOrderProcessHelper, "handoverFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setFbbActivated(true);
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest).offline(true);
    OrderDownloadRequest orderDownloadRequest = builder.build();
    Mockito.doThrow(Exception.class).when(this.businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId()));
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(orderDownloadRequest);
    Mockito.verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId()));
    Assertions.assertFalse(csvModel.getHeaderList().contains(ORDER_HANDED_OVER));
  }

  @Test
  public void getDirectory() throws Exception {
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request("123");
    OrderDownloadRequest request = builder.build();
    String directory = bulkOrderProcessHelper.getDirectory(request);
    Assertions.assertTrue(directory.endsWith(request.getRequestId()));
    Assertions.assertTrue(directory.startsWith(ProcessorUtils.BULK_ORDER_DOWNLOAD_DIR));

  }

  @Test
  public void getEmailParams() throws Exception {
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request("123");
    builder.username("kesha@gmail.com");
    builder.merchant("M-123");
    OrderDownloadRequest request = builder.build();
    Map<String, Object> emailParams = bulkOrderProcessHelper.getEmailParams(request, request.getLanguage());
    Assertions.assertEquals("kesha", emailParams.get("name"));
    Assertions.assertEquals(request.getMerchantId(), emailParams.get("businessPartnerCode"));
    Assertions.assertEquals(request.getRequestId(), emailParams.get("reqId"));
    Assertions.assertTrue(emailParams.containsKey(EmailConstants.TEMPLATE_ID_PARAM));
    Assertions.assertTrue(emailParams.containsKey(EmailConstants.MAIL_SENDER_PARAM));
    Assertions.assertTrue(emailParams.containsKey(EmailConstants.MAIL_SUBJECT_PARAM));
  }

  @Test
  public void getRecordsUpdated() throws Exception {
    List<OrderItemSummaryResponse> responseList = new ArrayList<>();
    responseList.add(new OrderItemSummaryResponse());
    BulkOrderResponse response = new BulkOrderResponse(responseList);
    int recordsUpdated = bulkOrderProcessHelper.getRecordsUpdated(response);
    Assertions.assertEquals(responseList.size(), recordsUpdated);
  }

  @Test
  public void generateCsvFileTest()
      throws InvocationTargetException, NoSuchMethodException, IOException, IllegalAccessException {
    List<OrderItemSummaryResponse> responseList = new ArrayList<>();
    responseList.add(new OrderItemSummaryResponse());
    BulkOrderResponse response = new BulkOrderResponse(responseList);
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest);
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(builder.build());
    byte[] arr = bulkOrderProcessHelper.generateCsvFile(csvModel, response);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED);
    Assertions.assertNotNull(arr);
  }

  @Test
  public void getCsvHeadersMapReturnOffline_switchOffShowSalesmanInfo() throws Exception {
    ReflectionTestUtils.setField(bulkOrderProcessHelper, "handoverFeatureSwitch", Boolean.FALSE);
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest).offline(Boolean.TRUE);
    OrderDownloadRequest orderDownloadRequest = builder.build();
    orderDownloadRequest.setShowSalesmanInfo(true);
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(orderDownloadRequest);
    Assertions.assertTrue(csvModel.getHeaderList().contains(SALESMAN_EMAIL));
    Assertions.assertFalse(csvModel.getHeaderList().contains(ORDER_HANDED_OVER));
  }

  @Test
  public void getCsvHeadersMapReturnOffline_switchOnFbbAndCncSellerHeadersWithPaymentAndSalesman() throws Exception {
    ReflectionTestUtils.setField(bulkOrderProcessHelper, "handoverFeatureSwitch", Boolean.TRUE);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setFbbActivated(true);
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(true);
    profileResponse.setCompany(company);
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest).offline(Boolean.TRUE);
    builder.showPaymentMethodColumn(true);
    OrderDownloadRequest orderDownloadRequest = builder.build();
    orderDownloadRequest.setShowSalesmanInfo(true);
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId())))
        .thenReturn(profileResponse);
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(orderDownloadRequest);
    Mockito.verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId()));
    Assertions.assertTrue(csvModel.getHeaderList().contains(PAYMENT_DISPLAY_NAME));
    Assertions.assertTrue(csvModel.getHeaderList().contains(SALESMAN_EMAIL));
    Assertions.assertTrue(csvModel.getHeaderList().contains(ORDER_HANDED_OVER));
  }

  @Test
  public void getCsvHeadersMapReturnOffline_switchOnFbbAndCncSellerHeadersWithSalesmanOnly() throws Exception {
    ReflectionTestUtils.setField(bulkOrderProcessHelper, "handoverFeatureSwitch", Boolean.TRUE);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setFbbActivated(true);
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(true);
    profileResponse.setCompany(company);
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest).offline(Boolean.TRUE);
    OrderDownloadRequest orderDownloadRequest = builder.build();
    orderDownloadRequest.setShowSalesmanInfo(true);
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId())))
        .thenReturn(profileResponse);
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(orderDownloadRequest);
    Mockito.verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId()));
    Assertions.assertTrue(csvModel.getHeaderList().contains(SALESMAN_EMAIL));
    Assertions.assertTrue(csvModel.getHeaderList().contains(ORDER_HANDED_OVER));
    Assertions.assertFalse(csvModel.getHeaderList().contains(PAYMENT_DISPLAY_NAME));
  }

  @Test
  public void getCsvHeadersMapReturnOffline_switchOnNonFbbSellerHeadersWithSalesman() throws Exception {
    ReflectionTestUtils.setField(bulkOrderProcessHelper, "handoverFeatureSwitch", Boolean.TRUE);
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest).offline(Boolean.TRUE);
    OrderDownloadRequest orderDownloadRequest = builder.build();
    orderDownloadRequest.setShowSalesmanInfo(true);
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId())))
        .thenReturn(new ProfileResponse());
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(orderDownloadRequest);
    Assertions.assertTrue(csvModel.getHeaderList().contains(SALESMAN_EMAIL));
    Assertions.assertFalse(csvModel.getHeaderList().contains(ORDER_HANDED_OVER));
    Mockito.verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId()));
  }

  @Test
  public void getCsvHeadersMapReturnOffline_switchOnNonFbbSellerHeadersWithPaymentMethod() throws Exception {
    ReflectionTestUtils.setField(bulkOrderProcessHelper, "handoverFeatureSwitch", Boolean.TRUE);
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest).offline(Boolean.TRUE);
    builder.showPaymentMethodColumn(true);
    OrderDownloadRequest orderDownloadRequest = builder.build();
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId())))
        .thenReturn(new ProfileResponse());
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(orderDownloadRequest);
    Mockito.verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId()));
    Assertions.assertTrue(csvModel.getHeaderList().contains(PAYMENT_DISPLAY_NAME));
    Assertions.assertFalse(csvModel.getHeaderList().contains(ORDER_HANDED_OVER));
    Assertions.assertFalse(csvModel.getHeaderList().contains(SALESMAN_EMAIL));
  }

  @Test
  public void getCsvHeadersMapReturnOffline_switchOnNonFbbSellerHeadersWithPaymentMethodAndSalesman() throws Exception {
    ReflectionTestUtils.setField(bulkOrderProcessHelper, "handoverFeatureSwitch", Boolean.TRUE);
    OrderDownloadRequest.OrderBuilder builder = new OrderDownloadRequest.OrderBuilder();
    builder.request(DEFAULT_REQUEST_ID);
    builder.orderRequest(orderItemSummaryRequest).offline(Boolean.TRUE);
    builder.showPaymentMethodColumn(true);
    OrderDownloadRequest orderDownloadRequest = builder.build();
    orderDownloadRequest.setShowSalesmanInfo(true);
    Mockito.when(this.businessPartnerRepository.filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId())))
        .thenReturn(new ProfileResponse());
    BulkCsvModel csvModel = bulkOrderProcessHelper.getCsvHeadersMap(orderDownloadRequest);
    Mockito.verify(this.businessPartnerRepository).filterByBusinessPartnerCodeV2(eq(Constant.STORE_ID), eq(orderDownloadRequest.getMerchantId()));
    Assertions.assertTrue(csvModel.getHeaderList().contains(PAYMENT_DISPLAY_NAME));
    Assertions.assertTrue(csvModel.getHeaderList().contains(SALESMAN_EMAIL));
    Assertions.assertFalse(csvModel.getHeaderList().contains(ORDER_HANDED_OVER));
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(systemParameterConfigService);
    verifyNoMoreInteractions(businessPartnerRepository);
  }
}
