package com.gdn.mta.bulk.service.download;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.config.IprSourceDisplayNameProperties;
import com.gdn.mta.bulk.models.download.IPRProductListRequest;
import com.gdn.mta.bulk.models.download.IPRProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkIprProductDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.IprProductsResponse;
import com.gdn.mta.bulk.repository.download.ProductDistributionTaskRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.eq;

public class BulkDownloadIprProductServiceBeanTest {

  @InjectMocks
  private BulkDownloadIprProductsServiceBean bulkDownloadIprProductsServiceBean;

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  private IprSourceDisplayNameProperties iprSourceDisplayNameProperties;

  private IPRProductsDownloadRequest request;

  private GdnRestListResponse<IprProductsResponse> response;

  private GdnRestListResponse<IprProductsResponse> response2;

  private static final String REQUEST_ID = "REQUEST_ID";

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    Map<String, String> iprSourceDisplayNameMap = new HashMap<>();
    iprSourceDisplayNameMap.put("CUSTOMER_REPORT", "Laporan pelanggan");
    iprSourceDisplayNameMap.put("RANDOM_SAMPLE", "Sampel acak");
    iprSourceDisplayNameMap.put("BRAND_REPORT", "Laporan brand");
    iprSourceDisplayNameProperties.map = iprSourceDisplayNameMap;
    ReflectionTestUtils.setField(bulkDownloadIprProductsServiceBean, "iprFetchBatchSize", 1);
    request = new IPRProductsDownloadRequest();
    request.setRequestId(REQUEST_ID);
    response = new GdnRestListResponse<>();
    response2 = new GdnRestListResponse<>();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(productDistributionTaskRepository);
  }

  @Test
  void getDataTest() throws Exception {
    response.setSuccess(true);
    response.setContent(Collections.singletonList(IprProductsResponse.builder().build()));
    response.setPageMetaData(new PageMetaData(1, 0, 2));
    Mockito.when(productDistributionTaskRepository.getIprProductsList(eq(REQUEST_ID),
        Mockito.any(IPRProductListRequest.class), eq(0), eq(1))).thenReturn(response);
    Mockito.when(productDistributionTaskRepository.getIprProductsList(eq(REQUEST_ID),
        Mockito.any(IPRProductListRequest.class), eq(1), eq(1))).thenReturn(response);
    BulkIprProductDownloadResponse bulkDataResponse =
        (BulkIprProductDownloadResponse) bulkDownloadIprProductsServiceBean.getData(request);
    Assertions.assertEquals(2, bulkDataResponse.getIprProductsResponses().size());
    Mockito.verify(productDistributionTaskRepository, Mockito.times(2))
        .getIprProductsList(eq(REQUEST_ID), Mockito.any(IPRProductListRequest.class),
            Mockito.anyInt(), Mockito.anyInt());
  }

  @Test
  void getDataTest_unequalDataCountInPage() throws Exception {
    ReflectionTestUtils.setField(bulkDownloadIprProductsServiceBean, "iprFetchBatchSize", 2);
    response.setSuccess(true);
    response.setContent(Arrays.asList(IprProductsResponse.builder().build(),
        IprProductsResponse.builder().build()));
    response.setPageMetaData(new PageMetaData(2, 0, 3));
    response2.setSuccess(true);
    response2.setContent(Collections.singletonList(IprProductsResponse.builder().build()));
    response2.setPageMetaData(new PageMetaData(2, 1, 3));
    Mockito.when(productDistributionTaskRepository.getIprProductsList(eq(REQUEST_ID),
        Mockito.any(IPRProductListRequest.class), eq(0), eq(2))).thenReturn(response);
    Mockito.when(productDistributionTaskRepository.getIprProductsList(eq(REQUEST_ID),
        Mockito.any(IPRProductListRequest.class), eq(1), eq(2))).thenReturn(response2);
    BulkIprProductDownloadResponse bulkDataResponse =
        (BulkIprProductDownloadResponse) bulkDownloadIprProductsServiceBean.getData(request);
    Assertions.assertEquals(3, bulkDataResponse.getIprProductsResponses().size());
    Mockito.verify(productDistributionTaskRepository, Mockito.times(2))
        .getIprProductsList(eq(REQUEST_ID), Mockito.any(IPRProductListRequest.class),
            Mockito.anyInt(), Mockito.anyInt());
  }
}