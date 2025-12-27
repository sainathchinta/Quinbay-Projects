package com.gdn.mta.bulk.service.download;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.MasterProductDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterProductResponse;
import com.gdn.mta.bulk.repository.ActiveProductRepository;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;

public class BulkDownloadMasterProductServiceBeanTest {

  @Mock
  private ActiveProductRepository activeProductRepository;

  @InjectMocks
  private BulkDownloadMasterProductServiceBean bulkDownloadMasterProductServiceBean;

  private List<MasterProductResponse> responseList;
  private static final String PRODUCT_CODE_1 = "MTA-001";
  private static final String PRODUCT_CODE_2 = "MTA-002";

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    responseList = new ArrayList<>();
    MasterProductResponse masterProductResponse = new MasterProductResponse();
    masterProductResponse.setProductCode(PRODUCT_CODE_1);
    responseList.add(masterProductResponse);

    masterProductResponse = new MasterProductResponse();
    masterProductResponse.setProductCode(PRODUCT_CODE_2);
    responseList.add(masterProductResponse);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(activeProductRepository);
  }

  @Test
  public void getData_data_exist_success() throws Exception {
    when(activeProductRepository.getActiveProductDetails(Mockito.any(MasterProductDownloadRequest.class)))
        .thenReturn(responseList);
    MasterProductDownloadRequest.MasterProductDownloadBuilder masterProductDownloadBuilder =
        new MasterProductDownloadRequest.MasterProductDownloadBuilder();
    masterProductDownloadBuilder.bulkProcessType(BulkProcessEntity.MASTER_PRODUCT);
    masterProductDownloadBuilder.emailCC("kesha@xyz.com");
    masterProductDownloadBuilder.emailTo("kesha@xyz.com");
    masterProductDownloadBuilder.filename("test.xlsx");
    masterProductDownloadBuilder.downloadType(DownloadType.ALL);
    masterProductDownloadBuilder.merchant("m-123");
    masterProductDownloadBuilder.request("123");
    BulkDownloadRequest request = masterProductDownloadBuilder.build();
    BulkDataResponse dataResponse = bulkDownloadMasterProductServiceBean.getData(request);
    Assertions.assertTrue(dataResponse instanceof BulkMasterProductResponse);
    BulkMasterProductResponse response = (BulkMasterProductResponse) dataResponse;
    Assertions.assertFalse(response.getResponseList().isEmpty());
    Assertions.assertEquals(responseList.size(), response.getResponseList().size());
    verify(activeProductRepository).getActiveProductDetails(Mockito.any(MasterProductDownloadRequest.class));
  }

  @Test
  public void getData_NullOrder_WriteLog() throws Exception {
    List<MasterProductResponse> responses = new ArrayList<>();
    when(activeProductRepository.getActiveProductDetails(Mockito.any(MasterProductDownloadRequest.class)))
        .thenReturn(responses);
    MasterProductDownloadRequest.MasterProductDownloadBuilder masterProductDownloadBuilder =
        new MasterProductDownloadRequest.MasterProductDownloadBuilder();
    masterProductDownloadBuilder.bulkProcessType(BulkProcessEntity.MASTER_PRODUCT);
    masterProductDownloadBuilder.emailCC("kesha@xyz.com");
    masterProductDownloadBuilder.emailTo("kesha@xyz.com");
    masterProductDownloadBuilder.filename("test.xlsx");
    masterProductDownloadBuilder.downloadType(DownloadType.ALL);
    masterProductDownloadBuilder.merchant("m-123");
    masterProductDownloadBuilder.request("123");
    BulkDownloadRequest request = masterProductDownloadBuilder.build();
    BulkDataResponse dataResponse = bulkDownloadMasterProductServiceBean.getData(request);
    Assertions.assertTrue(dataResponse instanceof BulkMasterProductResponse);
    BulkMasterProductResponse response = (BulkMasterProductResponse) dataResponse;
    Assertions.assertTrue(response.getResponseList().isEmpty());
    Assertions.assertEquals(0, response.getResponseList().size());
    verify(activeProductRepository).getActiveProductDetails(Mockito.any(MasterProductDownloadRequest.class));
  }

}
