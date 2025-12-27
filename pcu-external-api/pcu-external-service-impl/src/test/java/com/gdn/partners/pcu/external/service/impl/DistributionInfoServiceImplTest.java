package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.web.model.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class DistributionInfoServiceImplTest {
  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private PBPFeign pbpFeign;

  @InjectMocks
  private DistributionInfoServiceImpl distributionInfoService;

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(pcbFeign);
  }

  @Test
  void getDistributionInfo_success_returnsPage() {
    String productCode = "PROD001";
    boolean needDistributionInfoResponse = true;
    int page = 0;
    int size = 10;
    DistributionInfoPerSkuResponse dto = new DistributionInfoPerSkuResponse();
    List<DistributionInfoPerSkuResponse> content = Collections.singletonList(dto);
    GdnRestListResponse<DistributionInfoPerSkuResponse> mockResponse = new GdnRestListResponse<>();
    mockResponse.setSuccess(true);
    mockResponse.setContent(content);
    mockResponse.setPageMetaData(
        new com.gdn.common.web.wrapper.response.PageMetaData(size, page, 100L));
    when(pcbFeign.getDistributionInfo(productCode, needDistributionInfoResponse, page,
        size)).thenReturn(mockResponse);
    Page<DistributionInfoPerSkuResponse> result =
        distributionInfoService.getDistributionInfo(productCode, needDistributionInfoResponse, page,
            size);
    assertNotNull(result);
    assertEquals(content.size(), result.getContent().size());
    assertEquals(100L, result.getTotalElements());
    verify(pcbFeign, times(1)).getDistributionInfo(productCode, needDistributionInfoResponse, page,
        size);
  }

  @Test
  void updateDistributionInfo_success() {
    String productCode = "PROD001";
    DistributionInfoUpdateRequest distributionInfoUpdateRequest = new DistributionInfoUpdateRequest();
    when(pbpFeign.updateDistributionInfo(productCode, distributionInfoUpdateRequest))
        .thenReturn(new GdnBaseRestResponse(true));
    distributionInfoService.updateDistributionInfo(productCode,distributionInfoUpdateRequest);
    verify(pbpFeign, times(1))
        .updateDistributionInfo(productCode, distributionInfoUpdateRequest);
  }
}
