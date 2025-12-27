package com.gdn.partners.pcu.external.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.DistributionInfoApiPath;
import com.gdn.partners.pcu.external.service.DistributionInfoService;
import com.gdn.partners.pcu.external.web.model.request.DistributionInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemUomInfoWebRequest;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.Collections;
import java.util.List;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith(MockitoExtension.class)
public class DistributionInfoControllerTest {

  private static final String REQUEST_ID = "REQUEST_ID";
  @Mock
  private DistributionInfoService distributionInfoService;

  @InjectMocks
  private DistributionInfoController distributionInfoController;

  @Mock
  private ErrorController errorController;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  private MockMvc mockMvc;

  private static final String PRODUCT_CODE = "PROD123";

  @BeforeEach
  public void setUp() {
    mockMvc = MockMvcBuilders.standaloneSetup(distributionInfoController)
        .setControllerAdvice(errorController)
        .build();
  }
  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(distributionInfoService);
    Mockito.verifyNoMoreInteractions(mandatoryParameterHelper);
  }

  @Test
  public void getDistributionInfo_shouldReturnPagedResponse() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    DistributionInfoPerSkuResponse dto = new DistributionInfoPerSkuResponse();
    Page<DistributionInfoPerSkuResponse> page =
        new PageImpl<>(Collections.singletonList(dto), PageRequest.of(0, 50), 1);
    when(distributionInfoService.getDistributionInfo(PRODUCT_CODE, false, 0, 50)).thenReturn(page);
    mockMvc.perform(get(DistributionInfoApiPath.BASE_PATH + DistributionInfoApiPath.GET_DISTRIBUTION_INFO, PRODUCT_CODE)
            .param("needDistributionInfoResponse", "false")
            .param("page", "0")
            .param("size", "50")
            .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(distributionInfoService).getDistributionInfo(PRODUCT_CODE, false, 0, 50);
  }

  @Test
  public void updateDistributionInfo_success() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    DistributionInfoWebRequest distributionInfoWebRequest = new DistributionInfoWebRequest();
    distributionInfoWebRequest.setProductItems(
        List.of(ProductItemUomInfoWebRequest.builder().build()));
    doNothing().when(distributionInfoService).updateDistributionInfo(anyString(), any());
    mockMvc.perform(
            post(DistributionInfoApiPath.BASE_PATH + DistributionInfoApiPath.UPDATE_DISTRIBUTION_INFO,
                PRODUCT_CODE).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
                .content(new ObjectMapper().writeValueAsString(distributionInfoWebRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(distributionInfoService).updateDistributionInfo(anyString(), any());
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }
}
