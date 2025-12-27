package com.gdn.mta.bulk.service.download;


import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.gdn.mta.bulk.service.PickupPointService;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.StoreCopyUploadTemplateResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;

public class BulkStoreCopyUploadTemplateServiceBeanTest {

  @InjectMocks
  private BulkStoreCopyUploadTemplateServiceBean bulkStoreCopyUploadTemplateServiceBean;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private PickupPointService pickupPointService;

  private static final String PICKUP_POINT_CODE = "pp-code";
  private PickupPointResponse pickupPointResponse;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);

    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(businessPartnerRepository);
  }

  @Test
  public void getData() throws Exception {
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.any(), Mockito.any()))
        .thenReturn(getProfile());
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.STORE_COPY_UPLOAD_TEMPLATE);
    bulkDownloadRequest.setRequestId(BulkProcessEntity.STORE_COPY_UPLOAD_TEMPLATE.name());
    StoreCopyUploadTemplateResponse data =
        (StoreCopyUploadTemplateResponse) bulkStoreCopyUploadTemplateServiceBean.getData(bulkDownloadRequest);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(Mockito.any(), Mockito.any());
    Assertions.assertEquals(data.getPickupPoints().size(), 1);
  }

  public ProfileResponse getProfile() {
    ProfileResponse response = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setOfflineToOnlineFlag(false);
    companyDTO.setInventoryFulfillment("BL");
    List<PickupPointDTO> pickupPointDTOList = new ArrayList<>();
    PickupPointDTO pickupPointDTO = new PickupPointDTO();
    pickupPointDTO.setName("pickup point1");
    pickupPointDTO.setCode("123");
    pickupPointDTOList.add(pickupPointDTO);
    response.setCompany(companyDTO);
    response.setPickupPoints(pickupPointDTOList);
    response.setBusinessPartnerCode("M-123");
    return response;
  }
}