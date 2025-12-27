package com.gdn.mta.bulk.service.download;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.MerchantConfigurationDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkConfigSummaryResponse;
import com.gdn.mta.bulk.service.BulkConfigurationService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;

public class BulkConfigurationMerchantSummaryServiceBeanTest {

  private static final int MAX_SIZE = 1;
  private static final String STORE_ID = "storeId";
  private static final String CODE = "code";
  private static final String NAME = "name";
  private static final String REVIEW_CONFIG = "status";
  private static final String SEARCH_KEY = "searchKey";
  private static final String SORT_ORDER = "sortOrder";
  private static final String USERNAME = "user-name";
  private static final String REQUEST_ID = "request-id";
  private static final String LANGUAGE = "eng";
  private static final String FILENAME = "file-name";
  private static final String CONFIG_TYPE = "configType";

  @InjectMocks
  private BulkConfigurationMerchantSummaryServiceBean bulkConfigurationMerchantSummaryService;

  @Mock
  private BulkConfigurationService bulkConfigurationService;

  private BulkDownloadRequest bulkDownloadRequest;
  private ConfigurationFilterRequest configurationFilterRequest;
  private BulkConfigDataResponse bulkConfigDataResponse;
  private MerchantConfigurationFilterResponse merchantConfigurationFilterResponse;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    bulkConfigDataResponse = BulkConfigDataResponse.builder().code(CODE).name(NAME).reviewConfig(REVIEW_CONFIG).build();

    configurationFilterRequest =
        ConfigurationFilterRequest.builder().categoryCode(CODE).reviewConfig(REVIEW_CONFIG).sortOrder(SORT_ORDER)
            .searchKey(SEARCH_KEY).build();

    MerchantConfigurationDownloadRequest.MerchantConfigurationDownloadRequestBuilder builder =
        new MerchantConfigurationDownloadRequest.MerchantConfigurationDownloadRequestBuilder();
    bulkDownloadRequest =
        builder.storeId(STORE_ID).configType(CONFIG_TYPE).categoryCode(CODE).sortOrder(SORT_ORDER).searchKey(SEARCH_KEY)
            .reviewConfig(REVIEW_CONFIG).dataList(Arrays.asList(CODE)).downloadType(DownloadType.ALL)
            .fileType(FileType.XLSX).bulkProcessType(BulkProcessEntity.CONFIGURATION_MERCHANT_SUMMARY)
            .directDownload(false).filename(FILENAME).emailCC(USERNAME).emailTo(USERNAME).username(USERNAME)
            .language(LANGUAGE).request(REQUEST_ID).build();

    merchantConfigurationFilterResponse =
        MerchantConfigurationFilterResponse.builder().merchantCode(CODE).merchantName(NAME).reviewConfig(REVIEW_CONFIG)
            .build();

    ReflectionTestUtils.setField(bulkConfigurationMerchantSummaryService, "bulkConfigurationDownloadSize", MAX_SIZE);
  }

  @Test
  public void getDataByCodeList() throws Exception {
    Mockito.when(bulkConfigurationService
        .fetchConfigDetailsByCodes(STORE_ID, CONFIG_TYPE, bulkDownloadRequest, Arrays.asList(CODE)))
        .thenReturn(Arrays.asList(bulkConfigDataResponse));
    BulkConfigSummaryResponse response =
        (BulkConfigSummaryResponse) bulkConfigurationMerchantSummaryService.getData(bulkDownloadRequest);
    Mockito.verify(bulkConfigurationService)
        .fetchConfigDetailsByCodes(STORE_ID, CONFIG_TYPE, bulkDownloadRequest, Arrays.asList(CODE));
    Assertions.assertEquals(Arrays.asList(bulkConfigDataResponse), response.getBulkConfigDataResponseList());
  }

  @Test
  public void getDataByRequestBody() throws Exception {
    MerchantConfigurationDownloadRequest.MerchantConfigurationDownloadRequestBuilder builder =
        new MerchantConfigurationDownloadRequest.MerchantConfigurationDownloadRequestBuilder();
    bulkDownloadRequest =
        builder.storeId(STORE_ID).configType(CONFIG_TYPE).categoryCode(CODE).sortOrder(SORT_ORDER).searchKey(SEARCH_KEY)
            .reviewConfig(REVIEW_CONFIG).dataList(null).downloadType(DownloadType.ALL).fileType(FileType.XLSX)
            .bulkProcessType(BulkProcessEntity.CONFIGURATION_MERCHANT_SUMMARY).directDownload(false).filename(FILENAME)
            .emailCC(USERNAME).emailTo(USERNAME).username(USERNAME).language(LANGUAGE).request(REQUEST_ID).build();
    Mockito.when(bulkConfigurationService
        .getMerchantConfigurationList(eq(STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID), eq(REQUEST_ID),
            eq(USERNAME), Mockito.any(ConfigurationFilterRequest.class), Mockito.anyInt(), eq(MAX_SIZE))).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(merchantConfigurationFilterResponse),
            new PageMetaData(0, MAX_SIZE, 2L), null));
    BulkConfigSummaryResponse response =
        (BulkConfigSummaryResponse) this.bulkConfigurationMerchantSummaryService.getData(bulkDownloadRequest);
    Mockito.verify(bulkConfigurationService, Mockito.times(2))
        .getMerchantConfigurationList(eq(STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID), eq(REQUEST_ID),
            eq(USERNAME), Mockito.any(ConfigurationFilterRequest.class), Mockito.anyInt(), eq(MAX_SIZE));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(CODE, response.getBulkConfigDataResponseList().get(0).getCode());
    Assertions.assertEquals(NAME, response.getBulkConfigDataResponseList().get(0).getName());
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.bulkConfigurationService);
  }
}
