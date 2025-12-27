package com.gdn.partners.pcu.master.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.client.model.DimensionFilterRequest;
import com.gdn.partners.pcu.master.client.model.DimensionMappingResponse;
import com.gdn.partners.pcu.master.client.model.DimensionResponse;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.DimensionApiPath;
import com.gdn.partners.pcu.master.service.DimensionService;
import com.gdn.partners.pcu.master.web.model.request.DimensionWebRequest;
import com.gdn.partners.pcu.master.web.model.request.EditDimensionWebRequest;
import com.gdn.partners.pcu.master.web.model.request.ModifyDimensionMappingWebRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.List;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith(MockitoExtension.class)
class DimensionControllerTest {

  private static final String DIMENSION_CODE = "DM-10001";
  private static final String DIMENSION_NAME = "DIMENSION_NAME";
  private static final String DIMENSION_ENGLISH_NAME = "DIMENSION_ENGLISH_NAME";
  private static final byte[] DESCRIPTION = "DESCRIPTION".getBytes();
  private static final byte[] DESCRIPTION_ENGLISH = "DESCRIPTION_ENGLISH".getBytes();
  private static final String EXAMPLE = "EXAMPLE";
  private static final String ATTRIBUTE_CODE = "AT-10001";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String DIMENSION_NAME_PARAMETER = "dimensionName";

  @InjectMocks
  private DimensionController dimensionController;

  @Mock
  private DimensionService dimensionService;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  private MockMvc mockMvc;
  private DimensionResponse dimensionResponse;
  private DimensionMappingResponse dimensionMappingResponse;
  private DimensionFilterRequest dimensionFilterRequest;
  private DimensionWebRequest dimensionWebRequest;
  private EditDimensionWebRequest editDimensionWebRequest;
  private ObjectMapper objectMapper;
  private ModifyDimensionMappingWebRequest modifyDimensionMappingWebRequest;

  @BeforeEach
  void beforeEach() {
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.dimensionController).build();
    objectMapper = new ObjectMapper();
    dimensionResponse = new DimensionResponse();
    dimensionResponse.setDimensionCode(DIMENSION_CODE);
    dimensionFilterRequest = new DimensionFilterRequest();
    dimensionWebRequest  = DimensionWebRequest.builder().name(DIMENSION_NAME)
        .nameEnglish(DIMENSION_ENGLISH_NAME)
        .description(DESCRIPTION).descriptionEnglish(DESCRIPTION_ENGLISH)
        .example(EXAMPLE).build();
    dimensionMappingResponse = new DimensionMappingResponse();
    dimensionMappingResponse.setAttributeCode(ATTRIBUTE_CODE);
    modifyDimensionMappingWebRequest = new ModifyDimensionMappingWebRequest();
    editDimensionWebRequest = EditDimensionWebRequest.builder().nameEnglish(DIMENSION_NAME)
        .description(DESCRIPTION).descriptionEnglish(DESCRIPTION_ENGLISH)
        .example(EXAMPLE).build();
  }

  @AfterEach
  void teardown() {
    Mockito.verifyNoMoreInteractions(dimensionService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  void fetchDimensionDetailTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(dimensionService.fetchDimensionDetail(DIMENSION_CODE)).thenReturn(dimensionResponse);
    MockHttpServletRequestBuilder requestBuilder =
      get(DimensionApiPath.BASE_PATH + DimensionApiPath.FETCH_DIMENSION_DETAIL_BY_DIMENSION_CODE,
        DIMENSION_CODE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(dimensionService).fetchDimensionDetail(DIMENSION_CODE);
  }

  @Test
  void fetchDimensionListing() throws Exception {
    GdnRestListResponse<DimensionResponse> serviceResponse =
      new GdnRestListResponse<>(List.of(dimensionResponse), null, Constants.REQUEST_ID);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(dimensionService.fetchDimensionListing(dimensionFilterRequest, PAGE, SIZE)).thenReturn(
      serviceResponse);
    MockHttpServletRequestBuilder requestBuilder =
      post(DimensionApiPath.BASE_PATH + DimensionApiPath.FILTER).contentType(
          MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(dimensionFilterRequest))
        .param(Constants.PAGE, String.valueOf(PAGE)).param(Constants.SIZE, String.valueOf(SIZE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(dimensionService).fetchDimensionListing(dimensionFilterRequest, PAGE, SIZE);
  }

  @Test
  void saveNewDimensionTest() throws Exception {
    GdnBaseRestResponse serviceResponse = new GdnBaseRestResponse(true);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(dimensionService.save(Mockito.any())).thenReturn(
        serviceResponse);
    MockHttpServletRequestBuilder requestBuilder =
        post(DimensionApiPath.BASE_PATH + DimensionApiPath.CREATE).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(dimensionWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(dimensionService).save(Mockito.any());
  }

  @Test
  void fetchDimensionMappingTest() throws Exception {
    GdnRestListResponse<DimensionMappingResponse> serviceResponse =
        new GdnRestListResponse<>(List.of(dimensionMappingResponse), null, Constants.REQUEST_ID);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(dimensionService.fetchDimensionMapping(ATTRIBUTE_CODE, PAGE, SIZE)).thenReturn(
        serviceResponse);
    MockHttpServletRequestBuilder requestBuilder =
        get(DimensionApiPath.BASE_PATH + DimensionApiPath.DIMENSION_MAPPING_LIST, ATTRIBUTE_CODE)
            .param(Constants.PAGE, String.valueOf(PAGE)).param(Constants.SIZE, String.valueOf(SIZE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(dimensionService).fetchDimensionMapping(ATTRIBUTE_CODE, PAGE, SIZE);
  }

  @Test
  void modifyDimensionMappingTest() throws Exception {
    GdnRestListResponse<DimensionMappingResponse> serviceResponse =
        new GdnRestListResponse<>(List.of(dimensionMappingResponse), null, Constants.REQUEST_ID);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(dimensionService.modifyDimensionMapping(ATTRIBUTE_CODE, modifyDimensionMappingWebRequest)).thenReturn(serviceResponse);
    MockHttpServletRequestBuilder requestBuilder =
        post(DimensionApiPath.BASE_PATH + DimensionApiPath.UPDATE_DIMENSION_MAPPING_LIST,
            ATTRIBUTE_CODE).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(modifyDimensionMappingWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(dimensionService)
        .modifyDimensionMapping(ATTRIBUTE_CODE, modifyDimensionMappingWebRequest);
  }

  @Test
  void editDimensionTest() throws Exception {
    GdnBaseRestResponse serviceResponse = new GdnBaseRestResponse(true);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(dimensionService.edit(Mockito.any(), Mockito.any())).thenReturn(serviceResponse);
    MockHttpServletRequestBuilder requestBuilder =
        post(DimensionApiPath.BASE_PATH + DimensionApiPath.EDIT, DIMENSION_CODE).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(editDimensionWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(dimensionService).edit(Mockito.any(), Mockito.any());
  }

  @Test
  void validateDimensionTest() throws Exception {
    GdnRestSingleResponse<DimensionResponse> response = new GdnRestSingleResponse<>();
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(dimensionService.validate(DIMENSION_NAME)).thenReturn(response);
    MockHttpServletRequestBuilder requestBuilder =
        get(DimensionApiPath.BASE_PATH + DimensionApiPath.VALIDATE).param(DIMENSION_NAME_PARAMETER,
            DIMENSION_NAME);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(dimensionService).validate(DIMENSION_NAME);
  }
}