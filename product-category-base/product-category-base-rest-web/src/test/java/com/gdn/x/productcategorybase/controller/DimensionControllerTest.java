package com.gdn.x.productcategorybase.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.DimensionApiPath;
import com.gdn.x.productcategorybase.dto.request.DimensionFilterRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionRequest;
import com.gdn.x.productcategorybase.dto.response.DimensionMappingResponse;
import com.gdn.x.productcategorybase.dto.response.DimensionResponse;
import com.gdn.x.productcategorybase.service.DimensionMappingService;
import com.gdn.x.productcategorybase.service.DimensionMappingWrapperService;
import com.gdn.x.productcategorybase.service.DimensionService;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.web.servlet.MockMvc;

import java.util.ArrayList;
import java.util.List;

import java.util.Collections;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

public class DimensionControllerTest {

  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String REQUEST_ID = "REQ-001";
  private static final String STORE_ID = "10001";
  private static final String USERNAME = "developer";
  private static final String DIMENSION_CODE = "DIM-000";
  private static final String ATTRIBUTE_CODE = "AC-12345";
  private static final String ID = "ID";
  private static final int PAGE = 0;
  private static final int SIZE = 50;

  @InjectMocks
  private DimensionController controller;

  @Mock
  private DimensionService dimensionService;

  @Mock
  private DimensionMappingService dimensionMappingService;

  @Mock
  private DimensionMappingWrapperService dimensionMappingWrapperService;

  private static final ObjectMapper objectMapper = new ObjectMapper();

  private MockMvc mockMvc;
  private DimensionResponse dimensionResponse;
  private Pageable pageable;
  private DimensionMappingResponse dimensionMappingResponse;
  private Page<DimensionMappingResponse> dimensionMappingResponsePage;
  private List<DimensionMappingResponse> dimensionMappingResponseList;
  private DimensionMappingUpdateRequest dimensionMappingUpdateRequest;

  private static DimensionRequest dimensionRequest;

  private static DimensionFilterRequest dimensionFilterRequest;
  private static final String DIMENSION_NAME = "Name";
  private static final String DIMENSION_NAME_ENGLISH = "Name English";


  @BeforeEach
  public void setUp() {
    initMocks(this);
    this.mockMvc = standaloneSetup(this.controller).build();
    pageable = PageRequest.of(PAGE, SIZE);
    dimensionResponse = new DimensionResponse();
    dimensionResponse.setDimensionCode(DIMENSION_CODE);
    dimensionRequest = DimensionRequest.builder()
        .name(DIMENSION_NAME)
        .nameEnglish(DIMENSION_NAME_ENGLISH).build();

    dimensionFilterRequest = DimensionFilterRequest.builder().build();
    dimensionMappingResponse = new DimensionMappingResponse();
    dimensionMappingResponse.setAttributeCode(ATTRIBUTE_CODE);
    dimensionMappingResponse.setDimensionId(ID);
    dimensionMappingResponse.setMandatory(true);
    dimensionMappingResponseList = new ArrayList<>();
    dimensionMappingResponseList.add(dimensionMappingResponse);
    dimensionMappingResponsePage = new PageImpl<>(dimensionMappingResponseList);
    dimensionMappingUpdateRequest = new DimensionMappingUpdateRequest();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(dimensionService);
    Mockito.verifyNoMoreInteractions(dimensionMappingService);
    Mockito.verifyNoMoreInteractions(dimensionMappingWrapperService);
  }

  @Test
  public void fetchDimensionDetailTest() throws Exception {
    Mockito.when(dimensionService.fetchDimensionDetails(STORE_ID, DIMENSION_CODE))
      .thenReturn(dimensionResponse);
    this.mockMvc.perform(
        get(DimensionApiPath.BASE_PATH + DimensionApiPath.DETAIL, DIMENSION_CODE).param("storeId",
            STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
          .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(dimensionService).fetchDimensionDetails(STORE_ID, DIMENSION_CODE);
  }

  @Test
  public void saveNewDimensionTest_success() throws Exception {
    this.mockMvc.perform(post(DimensionApiPath.BASE_PATH + DimensionApiPath.SAVE)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(dimensionRequest)).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId",
                REQUEST_ID)
            .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(dimensionService).save(STORE_ID, dimensionRequest);
  }

  @Test
  public void filterDimensions_success() throws Exception {
    Mockito.when(
            dimensionService.filter(STORE_ID, dimensionFilterRequest, PageRequest.of(0,10)))
        .thenReturn(new PageImpl<>(Collections.singletonList(DimensionResponse.builder().build())));
    this.mockMvc.perform(post(DimensionApiPath.BASE_PATH + DimensionApiPath.FILTER).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(dimensionRequest)).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId",
                REQUEST_ID)
            .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(dimensionService).filter(STORE_ID,dimensionFilterRequest,PageRequest.of(0,10));
  }

  @Test
  public void fetchDimensionMappingTest() throws Exception {
    Mockito.when(dimensionMappingService.fetchDimensionMapping(ATTRIBUTE_CODE, STORE_ID, pageable))
        .thenReturn(dimensionMappingResponsePage);
    this.mockMvc.perform(get(DimensionApiPath.BASE_PATH + DimensionApiPath.DIMENSION_MAPPING_LIST,
        ATTRIBUTE_CODE).accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME)).andExpect(status().isOk());
    verify(this.dimensionMappingService).fetchDimensionMapping(ATTRIBUTE_CODE, STORE_ID, pageable);
  }

  @Test
  public void editDimensionTest() throws Exception {
    this.mockMvc.perform(post(DimensionApiPath.BASE_PATH + DimensionApiPath.EDIT)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(dimensionRequest)).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId",
                REQUEST_ID)
            .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(dimensionService).edit(STORE_ID, dimensionRequest);
  }

  @Test
  public void validateDimensionTest() throws Exception {
    Mockito.when(dimensionService.findByName(STORE_ID, DIMENSION_NAME))
        .thenReturn(dimensionResponse);
    this.mockMvc.perform(get(DimensionApiPath.BASE_PATH + DimensionApiPath.FIND_BY_NAME)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("dimensionName", DIMENSION_NAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(dimensionService).findByName(STORE_ID, DIMENSION_NAME);
  }

  @Test
  public void updateDimensionMappingTest() throws Exception {
    this.mockMvc.perform(
        post(DimensionApiPath.BASE_PATH + DimensionApiPath.UPDATE_DIMENSION_MAPPING,
          ATTRIBUTE_CODE).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(dimensionMappingUpdateRequest))
          .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
          .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(dimensionMappingWrapperService)
      .updateDimensionMapping(STORE_ID, ATTRIBUTE_CODE, dimensionMappingUpdateRequest);
  }
}
