package com.gdn.partners.pcu.master.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.gdn.partners.pcu.master.client.model.AllowedAttributeValueResponse;
import com.gdn.partners.pcu.master.client.model.AttributeResponse;
import com.gdn.partners.pcu.master.client.model.MasterAttributeRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.master.client.feign.PCBFeign;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.partners.pcu.master.model.attribute.AttributeValue;
import com.gdn.partners.pcu.master.model.attribute.AttributeValueUpdateModel;
import com.gdn.partners.pcu.master.model.request.AttributeValueAddServiceRequest;
import com.gdn.partners.pcu.master.model.request.AttributeValuesUpdateServiceRequest;
import com.gdn.partners.pcu.master.service.impl.exception.ClientException;
import com.gdn.partners.pcu.master.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.master.web.model.request.MasterAttributeDTO;
import com.gdn.partners.pcu.master.web.model.response.AttributeDetailWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeSortType;
import com.gdn.partners.pcu.master.web.model.response.AttributeValueWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeValuesWebResponse;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeSortTypeRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeAddRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeFilterRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

public class AttributeServiceImplTest {

  @Mock
  private PCBFeign pcbFeign;

  @InjectMocks
  private AttributeServiceImpl attributeService;

  @Captor
  private ArgumentCaptor<MasterAttributeUpdateRequest> masterAttributeUpdateRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<MasterAttributeAddRequest> masterAttributeAddRequestArgumentCaptor;


  private static final String ATTRIBUTE_CODE = "ATT-";
  private static final String REQUEST_ID = "requestId";
  private static final String NAME_ENGLISH = "name";
  private static final byte[] DESCRIPTION_ENGLISH = "description".getBytes();
  private static final String VALUE = "VALUE";
  private static final String PREDEFINED_ATTRIBUTE_CODE = "PRE-";
  private static final String ALLOWED_ATTRIBUTE_CODE = null;
  private static final int SEQUENCE = 0;
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 10;
  private static final Integer TOTAL_RECORDS = 1;
  private static final String ATTRIBUTE_VALUE_UPDATE_ID_1 = "id1";
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_1 = "value1";
  private static final String ATTRIBUTE_VALUE_UPDATE_CODE_1 = "code2";
  private static final Integer ATTRIBUTE_VALUE_UPDATE_SEQUENCE_1 = 1;
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_2 = "value2";
  private static final Integer ATTRIBUTE_VALUE_UPDATE_SEQUENCE_2 = 2;
  private static final String ATTRIBUTE_VALUE_UPDATE_ID_3 = "id3";
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_3 = "value3";
  private static final String ATTRIBUTE_VALUE_UPDATE_CODE_3 = "code3";
  private static final String EXAMPLE = "example";
  private static final String ATTRIBUTE_ID = "attributeId";
  private static final String USERNAME = "username";
  private static final String ID = "id";
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE_VALUE = "predefined_code value";
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "predefined_code";

  private GdnRestListResponse<AttributeValueResponse> attributeValueResponses;
  private GdnRestListResponse<AttributeResponse> attributeResponses;
  private AttributeResponse attributeResponse;
  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse;
  private AllowedAttributeValueResponse allowedAttributeValueResponse;
  private GdnRestSingleResponse gdnRestSingleResponse;


  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
    AttributeValueResponse
        attributeValueResponse = new AttributeValueResponse(PREDEFINED_ATTRIBUTE_CODE, VALUE, SEQUENCE, ALLOWED_ATTRIBUTE_CODE);
    List<AttributeValueResponse> attributeValueResponseList = new ArrayList<>();
    attributeValueResponseList.add(attributeValueResponse);
    PageMetaData pageMetaData = new PageMetaData(SIZE, PAGE, TOTAL_RECORDS);
    attributeValueResponses = new GdnRestListResponse<>(attributeValueResponseList, pageMetaData, REQUEST_ID);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    List<AttributeResponse> attributeResponseList = Arrays.asList(attributeResponse);
    attributeResponses = new GdnRestListResponse<>(attributeResponseList, pageMetaData, REQUEST_ID);

    predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue(PREDEFINED_ALLOWED_ATTRIBUTE_CODE_VALUE);
    predefinedAllowedAttributeValueResponse.setId(ID);
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueResponse.setSequence(SEQUENCE);

    allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setSequence(SEQUENCE);
    allowedAttributeValueResponse.setValue(String.valueOf(VALUE));

    this.attributeResponse = new AttributeResponse();
    this.attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    this.attributeResponse.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));
    this.attributeResponse.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));

    gdnRestSingleResponse = new GdnRestSingleResponse(null, null, true, this.attributeResponse, REQUEST_ID);
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(pcbFeign);
  }

  @Test
  void getAttributeValuesTest() {
    Mockito.when(pcbFeign.getAttributeValues(ATTRIBUTE_CODE, PAGE, SIZE, false, false)).thenReturn(attributeValueResponses);
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    Page<AttributeValue> response = attributeService.getAttributeValues(ATTRIBUTE_CODE, pageable, false, false);
    Mockito.verify(pcbFeign).getAttributeValues(ATTRIBUTE_CODE, PAGE, SIZE, false, false);
    Assertions.assertEquals(PREDEFINED_ATTRIBUTE_CODE, response.getContent().get(0).getPredefinedAllowedAttributeCode());
    Assertions.assertTrue(response.getContent().size() == TOTAL_RECORDS);
    Assertions.assertEquals(response.getTotalElements(), TOTAL_RECORDS.longValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getContent().get(0).getAttributeCode());
  }

  @Test
  void addAttribute() {
    MasterAttributeRequest masterAttributeRequest = new MasterAttributeRequest();
    GdnBaseRestResponse restResponse = new GdnBaseRestResponse(Boolean.TRUE);
    when(pcbFeign.saveMasterAttribute(masterAttributeRequest)).thenReturn(restResponse);
    GdnBaseRestResponse response = attributeService.addAttribute(masterAttributeRequest);
    assertTrue(response.isSuccess());
    verify(pcbFeign).saveMasterAttribute(masterAttributeRequest);
  }

  @Test
  void getAttributeDetailTest() {
    Mockito.when(pcbFeign.getAttributeInfo(ATTRIBUTE_CODE)).thenReturn(
        new GdnRestSingleResponse<MasterAttributeResponse>(
            new MasterAttributeResponse(NAME_ENGLISH, DESCRIPTION_ENGLISH, AttributeSortTypeRequest.MANUAL, EXAMPLE), REQUEST_ID));
    MasterAttributeResponse response = attributeService.getAttributeDetail(ATTRIBUTE_CODE);
    Mockito.verify(pcbFeign).getAttributeInfo(ATTRIBUTE_CODE);
    Assertions.assertEquals(NAME_ENGLISH, response.getNameEnglish());
  }

  @Test
  void updateAttributeTest() {
    MasterAttributeRequest masterAttributeRequest = new MasterAttributeRequest();
    MasterAttributeDTO masterAttributeDTO = new MasterAttributeDTO();
    GdnBaseRestResponse restResponse = new GdnBaseRestResponse(Boolean.TRUE);
    when(pcbFeign.updateMasterAttribute(masterAttributeRequest)).thenReturn(restResponse);
    GdnBaseRestResponse response = attributeService.updateAttribute(masterAttributeDTO);
    assertTrue(response.isSuccess());
    verify(pcbFeign).updateMasterAttribute(masterAttributeRequest);
  }

  @Test
  void updateAttributeValuesTest() {
    AttributeValueUpdateModel attributeValueUpdateModel1 = AttributeValueUpdateModel.builder()
        .id(ATTRIBUTE_VALUE_UPDATE_ID_1).allowedAttributeCode(ATTRIBUTE_VALUE_UPDATE_CODE_1)
        .value(ATTRIBUTE_VALUE_UPDATE_VALUE_1).sequence(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_1).build();
    AttributeValueUpdateModel attributeValueUpdateModel2 = AttributeValueUpdateModel.builder()
        .value(ATTRIBUTE_VALUE_UPDATE_VALUE_2).sequence(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_2).build();
    AttributeValueUpdateModel attributeValueUpdateModel3 = AttributeValueUpdateModel.builder()
        .id(ATTRIBUTE_VALUE_UPDATE_ID_3).allowedAttributeCode(ATTRIBUTE_VALUE_UPDATE_CODE_3)
        .value(ATTRIBUTE_VALUE_UPDATE_VALUE_3).build();
    AttributeValuesUpdateServiceRequest attributeValuesUpdateServiceRequest =
        AttributeValuesUpdateServiceRequest.builder()
            .sortType(AttributeSortType.MANUAL.toString())
            .attributeValues(Collections.singletonList(attributeValueUpdateModel1))
            .addedAttributeValues(Collections.singletonList(attributeValueUpdateModel2))
            .deletedAttributeValues(Collections.singletonList(attributeValueUpdateModel3))
            .build();
    when(pcbFeign.updateMasterAttributeValues(eq(ATTRIBUTE_CODE),
        any(MasterAttributeUpdateRequest.class))).thenReturn(new GdnBaseRestResponse(true));
    attributeService.updateAttributeValues(ATTRIBUTE_CODE, attributeValuesUpdateServiceRequest);
    verify(pcbFeign).updateMasterAttributeValues(eq(ATTRIBUTE_CODE),
        masterAttributeUpdateRequestArgumentCaptor.capture());
    assertEquals(ATTRIBUTE_VALUE_UPDATE_ID_1,
        masterAttributeUpdateRequestArgumentCaptor.getValue().getAttributeValues().get(0).getId());
    assertEquals(ATTRIBUTE_VALUE_UPDATE_VALUE_2,
        masterAttributeUpdateRequestArgumentCaptor.getValue().getAddedAttributeValues().get(0).getValue());
    assertEquals(ATTRIBUTE_VALUE_UPDATE_ID_3,
        masterAttributeUpdateRequestArgumentCaptor.getValue().getDeletedAttributeValues().get(0).getId());
  }

  @Test
  void addAttributeValueTest() {
    AttributeValueAddServiceRequest attributeValueAddServiceRequest = new AttributeValueAddServiceRequest();
    attributeValueAddServiceRequest.setValue(VALUE);
    attributeValueAddServiceRequest.setCreatedBy(USERNAME);
    attributeValueAddServiceRequest.setSequence(SEQUENCE);
    Date date = new Date();
    attributeValueAddServiceRequest.setCreatedDate(date);
    when(pcbFeign.addMasterAttributeValue(eq(ATTRIBUTE_CODE),
        any(MasterAttributeAddRequest.class))).thenReturn(new GdnRestSingleResponse<AttributeValueResponse>(
        new AttributeValueResponse(), REQUEST_ID));
    attributeService.addAttributeValue(ATTRIBUTE_CODE, attributeValueAddServiceRequest);
    verify(pcbFeign).addMasterAttributeValue(eq(ATTRIBUTE_CODE),
        masterAttributeAddRequestArgumentCaptor.capture());
    assertEquals(VALUE, masterAttributeAddRequestArgumentCaptor.getValue().getValue());
    assertEquals(USERNAME, masterAttributeAddRequestArgumentCaptor.getValue().getCreatedBy());
    assertEquals(date, masterAttributeAddRequestArgumentCaptor.getValue().getCreatedDate());
    assertEquals(SEQUENCE, masterAttributeAddRequestArgumentCaptor.getValue().getSequence());
  }

  @Test
  void addAttributeValue_withNullResponse() {
    when(pcbFeign.addMasterAttributeValue(eq(ATTRIBUTE_CODE), any(MasterAttributeAddRequest.class)))
        .thenReturn(new GdnRestSingleResponse<AttributeValueResponse>(null, REQUEST_ID));
    AttributeValueResponse response = null;
    try {
      response = attributeService.addAttributeValue(ATTRIBUTE_CODE, new AttributeValueAddServiceRequest());
    } catch (ClientException ex) {
    } finally {
      Mockito.verify(pcbFeign).addMasterAttributeValue(eq(ATTRIBUTE_CODE), any(MasterAttributeAddRequest.class));
      Assertions.assertNull(response);
    }
  }

  @Test
  void findByFilterTest(){
    Pageable pageable =  PageRequest.of(0, 10);
    PageMetaData pageMetaData = new PageMetaData(0, 0, 0);

    MasterAttributeFilterRequest masterAttributeFilterRequest = new MasterAttributeFilterRequest();
    GdnRestListResponse<MasterAttributeResponse> restResponse =
        new GdnRestListResponse<MasterAttributeResponse>(null, null, true,
            Collections.singletonList(new MasterAttributeResponse()), pageMetaData, null);
    Mockito.when(pcbFeign
        .getAttributesByAttributeFilter(masterAttributeFilterRequest, pageable.getPageNumber(),
            pageable.getPageSize())).thenReturn(restResponse);
    attributeService.findByFilter(masterAttributeFilterRequest, pageable);
    Mockito.verify(pcbFeign)
        .getAttributesByAttributeFilter(masterAttributeFilterRequest, pageable.getPageNumber(),
            pageable.getPageSize());

  }

  @Test
  void findByFilter_whenSuccessFalseTest() {
    Pageable pageable = PageRequest.of(0, 10);
    PageMetaData pageMetaData = new PageMetaData(0, 0, 0);
    MasterAttributeFilterRequest masterAttributeFilterRequest = new MasterAttributeFilterRequest();
    GdnRestListResponse<MasterAttributeResponse> restResponse =
        new GdnRestListResponse<MasterAttributeResponse>(null, null, false, null, pageMetaData,
            null);
    Mockito.when(pcbFeign
        .getAttributesByAttributeFilter(masterAttributeFilterRequest, pageable.getPageNumber(),
            pageable.getPageSize())).thenReturn(restResponse);
    try {
      attributeService.findByFilter(masterAttributeFilterRequest, pageable);
    } catch (ClientException ex) {
      Mockito.verify(pcbFeign)
          .getAttributesByAttributeFilter(masterAttributeFilterRequest, pageable.getPageNumber(),
              pageable.getPageSize());
    }
  }

  @Test
  void findByFilter_whenResponseNullTest() {
    Pageable pageable = PageRequest.of(0, 10);
    MasterAttributeFilterRequest masterAttributeFilterRequest = new MasterAttributeFilterRequest();
    Mockito.when(pcbFeign
        .getAttributesByAttributeFilter(masterAttributeFilterRequest, pageable.getPageNumber(),
            pageable.getPageSize())).thenReturn(null);
    try {
      attributeService.findByFilter(masterAttributeFilterRequest, pageable);
    } catch (ClientException ex) {
      Mockito.verify(pcbFeign)
          .getAttributesByAttributeFilter(masterAttributeFilterRequest, pageable.getPageNumber(),
              pageable.getPageSize());
    }
  }

  @Test
  void getAttributeValuesByAttributeCodesTest() {
    when(pcbFeign.getAttributeValuesByAttributeCodes(any(AttributeCodesRequest.class)))
        .thenReturn(attributeResponses);
    List<AttributeValuesWebResponse> responseList =
        attributeService.getAttributeValuesByAttributeCodes(Arrays.asList(ATTRIBUTE_CODE));
    verify(pcbFeign).getAttributeValuesByAttributeCodes(any(AttributeCodesRequest.class));
    assertEquals(ATTRIBUTE_CODE, responseList.get(0).getAttributeCode());
  }

  @Test
  void getAttributeValuesByCodesExceptionTest() {
    when(pcbFeign.getAttributeValuesByAttributeCodes(any(AttributeCodesRequest.class))).thenReturn(null);
    List<AttributeValuesWebResponse> response = null;
    try {
      response = attributeService.getAttributeValuesByAttributeCodes(Arrays.asList(ATTRIBUTE_CODE));
    } catch (ClientException e) {
    } finally {
      verify(pcbFeign).getAttributeValuesByAttributeCodes(any(AttributeCodesRequest.class));
      Assertions.assertNull(response);
    }
  }

  @Test
  void getPredefinedAllowedAttributesByAttributeIdAndValueTest(){
    PageMetaData pageMetaData = new PageMetaData(0, 0, 0);
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    List<PredefinedAllowedAttributeValueResponse> predefinedAttributeList = new ArrayList<>();
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueResponse.setSequence(SEQUENCE);
    predefinedAllowedAttributeValueResponse.setValue(VALUE);
    predefinedAttributeList.add(predefinedAllowedAttributeValueResponse);
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> predefinedAttributeResponses =
        new GdnRestListResponse<>(predefinedAttributeList, pageMetaData, REQUEST_ID);
    Mockito.when(pcbFeign.getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(ATTRIBUTE_ID, VALUE, 0,
        Integer.MAX_VALUE))
        .thenReturn(predefinedAttributeResponses);
    List<AttributeValueWebResponse> response =
        attributeService.getPredefinedAllowedAttributesByAttributeIdAndValue(ATTRIBUTE_ID, VALUE);
    Mockito.verify(pcbFeign).getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(ATTRIBUTE_ID, VALUE, 0,
        Integer.MAX_VALUE);
    Assertions.assertEquals(SEQUENCE, (int) response.get(0).getSequence());
    Assertions.assertEquals(VALUE, response.get(0).getValue());
  }

  @Test
  void getSpecificPredefinedAllowedAttributesByAttributeIdAndValueTest() {
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueResponse.setSequence(SEQUENCE);
    predefinedAllowedAttributeValueResponse.setValue(VALUE);
    predefinedAllowedAttributeValueResponse.setId(ATTRIBUTE_ID);
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> predefinedAttributeResponses =
        new GdnRestListResponse<>(Arrays.asList(predefinedAllowedAttributeValueResponse), new PageMetaData(),
            REQUEST_ID);
    Mockito.when(pcbFeign.getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue(ATTRIBUTE_ID, VALUE))
        .thenReturn(predefinedAttributeResponses);
    List<AttributeValueWebResponse> response =
        attributeService.getSpecificPredefinedAllowedAttributesByAttributeIdAndValue(ATTRIBUTE_ID, VALUE);
    Mockito.verify(pcbFeign).getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue(ATTRIBUTE_ID, VALUE);
    Assertions.assertEquals(SEQUENCE, (int) response.get(0).getSequence());
    Assertions.assertEquals(VALUE, response.get(0).getValue());
    Assertions.assertEquals(ATTRIBUTE_ID, response.get(0).getId());
  }

  @Test
  void getSpecificPredefinedAllowedAttributesByAttributeIdAndValueExceptionTest() {
    when(pcbFeign.getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue(ATTRIBUTE_ID, VALUE))
        .thenReturn(null);
    List<AttributeValueWebResponse> response = null;
    try {
      response = attributeService.getSpecificPredefinedAllowedAttributesByAttributeIdAndValue(ATTRIBUTE_ID, VALUE);
    } catch (ClientException e) {
      verify(pcbFeign).getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue(ATTRIBUTE_ID, VALUE);
      Assertions.assertNull(response);
    }
  }

  @Test
  void getSpecificPredefinedAllowedAttributesByAttributeIdAndValueInvalidStateExceptionTest() {
    when(pcbFeign.getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue(ATTRIBUTE_ID, VALUE))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(), REQUEST_ID));
    try {
      attributeService.getSpecificPredefinedAllowedAttributesByAttributeIdAndValue(ATTRIBUTE_ID, VALUE);
    } catch (InvalidStateException e) {
      Assertions.assertEquals(ErrorMessages.BRAND_REJECTED_ERR_MESSAGE, e.getMessage());
    } finally {
      verify(pcbFeign).getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue(ATTRIBUTE_ID, VALUE);
    }
  }

  @Test
  void getAttributeDetailsTest() {
    when(pcbFeign.getAttributeDetailAndValuesByAttributeCode(ATTRIBUTE_CODE)).thenReturn(gdnRestSingleResponse);
    AttributeDetailWebResponse attributeDetailWebResponse = attributeService.getAttributeDetails(ATTRIBUTE_CODE);
    verify(pcbFeign).getAttributeDetailAndValuesByAttributeCode(ATTRIBUTE_CODE);
    assertEquals(ATTRIBUTE_CODE, attributeDetailWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_CODE,
        attributeDetailWebResponse.getAttributeValues().get(0).getAllowedAttributeCode());
    assertEquals(PREDEFINED_ALLOWED_ATTRIBUTE_CODE,
        attributeDetailWebResponse.getAttributeValues().get(1).getPredefinedAllowedAttributeCode());
  }
}
