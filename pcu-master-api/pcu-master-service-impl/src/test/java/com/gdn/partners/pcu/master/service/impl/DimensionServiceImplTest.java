package com.gdn.partners.pcu.master.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.master.client.feign.PCBFeign;
import com.gdn.partners.pcu.master.client.model.DimensionFilterRequest;
import com.gdn.partners.pcu.master.client.model.DimensionRequest;
import com.gdn.partners.pcu.master.client.model.DimensionMappingResponse;
import com.gdn.partners.pcu.master.client.model.DimensionResponse;
import com.gdn.partners.pcu.master.client.model.ModifyDimensionMappingRequest;
import com.gdn.partners.pcu.master.web.model.request.DimensionMappingDTO;
import com.gdn.partners.pcu.master.web.model.request.DimensionWebRequest;
import com.gdn.partners.pcu.master.web.model.request.ModifyDimensionMappingWebRequest;
import com.gdn.partners.pcu.master.web.model.request.EditDimensionWebRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class DimensionServiceImplTest {
  private static final String DIMENSION_CODE = "DM-00001";
  private static final String DIMENSION_NAME = "DIMENSION_NAME";
  private static final String DIMENSION_NAME_ENGLISH = "DIMENSION_NAME_ENGLISH";
  private static final String EXAMPLE = "EXAMPLE";
  private static final byte[] DESCRIPTION = "DESCRIPTION".getBytes();
  private static final String ATTRIBUTE_CODE = "AT-00001";
  private static final int PAGE = 0;
  private static final int SIZE = 10;

  private static final String REQUEST_ID = "requestId";

  @InjectMocks
  private DimensionServiceImpl dimensionService;

  @Mock
  private PCBFeign pcbFeign;

  private DimensionFilterRequest dimensionFilterRequest;
  private DimensionResponse dimensionResponse;
  private DimensionMappingResponse dimensionMappingResponse;
  private ModifyDimensionMappingRequest modifyDimensionMappingRequest;
  private ModifyDimensionMappingWebRequest mappingRequest;

  @BeforeEach
  void beforeEach() {
    dimensionFilterRequest = new DimensionFilterRequest();
    dimensionResponse = new DimensionResponse();
    dimensionResponse.setDimensionCode(DIMENSION_CODE);
    dimensionMappingResponse = new DimensionMappingResponse();
    dimensionMappingResponse.setAttributeCode(ATTRIBUTE_CODE);
    modifyDimensionMappingRequest = new ModifyDimensionMappingRequest();
    mappingRequest = new ModifyDimensionMappingWebRequest();
  }

  @AfterEach
  void teardown() {
    Mockito.verifyNoMoreInteractions(pcbFeign);
  }

  @Test
  void fetchDimensionListingTest() {
    PageMetaData pageMetaData = new PageMetaData(0, 0, 0);
    GdnRestListResponse<DimensionResponse> restResponse =
      new GdnRestListResponse<>(null, null, true, Collections.singletonList(dimensionResponse),
        pageMetaData, null);
    Mockito.when(pcbFeign.fetchDimensionListing(PAGE, SIZE, dimensionFilterRequest))
      .thenReturn(restResponse);
    GdnRestListResponse<DimensionResponse> serviceResponse =
      dimensionService.fetchDimensionListing(dimensionFilterRequest, PAGE, SIZE);
    Mockito.verify(pcbFeign).fetchDimensionListing(PAGE, SIZE, dimensionFilterRequest);
    Assertions.assertEquals(1, serviceResponse.getContent().size());
  }

  @Test
  void fetchDimensionDetailTest() {
    GdnRestSingleResponse<DimensionResponse> response =
      new GdnRestSingleResponse<>(dimensionResponse, REQUEST_ID);
    when(pcbFeign.fetchDimensionDetail(DIMENSION_CODE)).thenReturn(response);
    DimensionResponse serviceResponse = dimensionService.fetchDimensionDetail(DIMENSION_CODE);
    Mockito.verify(pcbFeign).fetchDimensionDetail(DIMENSION_CODE);
    Assertions.assertEquals(DIMENSION_CODE, serviceResponse.getDimensionCode());
  }

  @Test
  void saveNewDimensionTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    DimensionWebRequest dimensionWebRequest =
        DimensionWebRequest.builder().name(DIMENSION_NAME).nameEnglish(DIMENSION_NAME_ENGLISH)
            .example(EXAMPLE).description(DESCRIPTION).descriptionEnglish(DESCRIPTION).build();
    DimensionRequest dimensionRequest =
        DimensionRequest.builder().name(DIMENSION_NAME).nameEnglish(DIMENSION_NAME_ENGLISH)
            .example(EXAMPLE).description(DESCRIPTION).descriptionEnglish(DESCRIPTION).build();
    when(pcbFeign.createDimension(dimensionRequest)).thenReturn(response);
    GdnBaseRestResponse finalResponse = dimensionService.save(dimensionWebRequest);
    Mockito.verify(pcbFeign).createDimension(dimensionRequest);
    Assertions.assertTrue(finalResponse.isSuccess());
  }

  @Test
  void fetchDimensionMappingTest() {
    PageMetaData pageMetaData = new PageMetaData(0, 0, 0);
    GdnRestListResponse<DimensionMappingResponse> response =
        new GdnRestListResponse<>(null, null, true,
            Collections.singletonList(dimensionMappingResponse), pageMetaData, null);
    when(pcbFeign.fetchDimensionMapping(ATTRIBUTE_CODE, PAGE, SIZE)).thenReturn(response);
    GdnRestListResponse<DimensionMappingResponse> serviceResponse =
        dimensionService.fetchDimensionMapping(ATTRIBUTE_CODE, PAGE, SIZE);
    Mockito.verify(pcbFeign).fetchDimensionMapping(ATTRIBUTE_CODE, PAGE, SIZE);
    Assertions.assertTrue(serviceResponse.getContent().stream().findFirst().isPresent());
  }

  @Test
  void modifyDimensionMappingTest() throws Exception {
    mappingRequest.getAddedDimensionMapping().add(new DimensionMappingDTO());
    modifyDimensionMappingRequest.getAddedDimensionMapping()
        .add(new com.gdn.partners.pcu.master.client.model.DimensionMappingDTO());
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    when(pcbFeign.modifyDimensionMapping(eq(ATTRIBUTE_CODE), any())).thenReturn(response);
    GdnBaseRestResponse serviceResponse =
        dimensionService.modifyDimensionMapping(ATTRIBUTE_CODE , mappingRequest);
    Mockito.verify(pcbFeign).modifyDimensionMapping(ATTRIBUTE_CODE, modifyDimensionMappingRequest);
    Assertions.assertTrue(serviceResponse.isSuccess());
  }

  @Test
  void editDimensionTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    EditDimensionWebRequest editDimensionWebRequest =
        EditDimensionWebRequest.builder().nameEnglish(DIMENSION_NAME_ENGLISH).example(EXAMPLE)
            .description(DESCRIPTION).descriptionEnglish(DESCRIPTION).build();
    DimensionRequest dimensionRequest =
        DimensionRequest.builder().nameEnglish(DIMENSION_NAME_ENGLISH).example(EXAMPLE)
            .description(DESCRIPTION).descriptionEnglish(DESCRIPTION).build();
    dimensionRequest.setDimensionCode(DIMENSION_CODE);
    when(pcbFeign.editDimension(dimensionRequest)).thenReturn(response);
    GdnBaseRestResponse finalResponse =
        dimensionService.edit(DIMENSION_CODE, editDimensionWebRequest);
    Mockito.verify(pcbFeign).editDimension(dimensionRequest);
    Assertions.assertTrue(finalResponse.isSuccess());
  }

  @Test
  void validateDimensionTest() {
    GdnRestSingleResponse<DimensionResponse> response =
        new GdnRestSingleResponse<>(null, REQUEST_ID);
    when(pcbFeign.findByName(DIMENSION_NAME)).thenReturn(response);
    GdnRestSingleResponse<DimensionResponse> finalResponse =
        dimensionService.validate(DIMENSION_NAME);
    Mockito.verify(pcbFeign).findByName(DIMENSION_NAME);
    Assertions.assertTrue(finalResponse.isSuccess());
    Assertions.assertNull(finalResponse.getValue());
  }
}
