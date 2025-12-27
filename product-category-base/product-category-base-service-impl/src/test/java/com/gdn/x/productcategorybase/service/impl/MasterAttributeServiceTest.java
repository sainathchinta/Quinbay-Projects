package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
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
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;
import com.gdn.common.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.x.productcategorybase.AttributeFilter;
import com.gdn.x.productcategorybase.AttributeSortType;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.dto.AttributeTypeDTO;
import com.gdn.x.productcategorybase.dto.AttributeValueDTO;
import com.gdn.x.productcategorybase.dto.AttributeValueUpdateDTO;
import com.gdn.x.productcategorybase.dto.MasterAttributeUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Dimension;
import com.gdn.x.productcategorybase.entity.DimensionMapping;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.exception.ValidationException;
import com.gdn.x.productcategorybase.repository.AttributeRepository;
import com.gdn.x.productcategorybase.repository.CategoryAttributeRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.DimensionMappingService;
import com.gdn.x.productcategorybase.service.DimensionService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.SystemParameterService;
import com.gdn.x.productcategorybase.util.ConverterUtil;

public class MasterAttributeServiceTest {

  private static final String STORE_ID = "10001";
  private static final String ATTRIBUTE_CODE = "ATT-CODE";
  private static final String CATEGORY_ID = "CATEGORY_ID";
  private static final String NOT_FOUND_ATTRIBUTE_WITH_ATTRIBUTE_CODE = "Not found attribute with attribute code ";
  private static final String ATTRIBUTE1_NAME = "ATTRIBUTE_1_NAME";
  private static final String ATTRIBUTE2_NAME = "ATTRIBUTE_2_NAME";
  private static final String ATTRIBUTE3_NAME = "ATTRIBUTE_3_NAME";
  private static final String DS_ATTRIBUTE_NAME = "DS_ATTRIBUTE_NAME";
  private static final String VALUE = "Value";
  private static final PageRequest DEFAULT_PAGE_REQUEST = PageRequest.of(0, 10);
  private static final String ATTRIBUTE_ID = "ATT-ID";
  private static final Integer SEQUENCE = 1;
  private static final VerificationMode AT_LEAST_ONCE = times(1);
  private static final String USER_NAME = "UserName";
  private static final String ATTRIBUTE_VALUE_UPDATE_DTO_ID_1 = "id1";
  private static final String ATTRIBUTE_VALUE_UPDATE_DTO_CODE_1 = "code1";
  private static final Integer ATTRIBUTE_VALUE_UPDATE_SEQUENCE_1 = 1;
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_1 = "value1";
  private static final String ATTRIBUTE_VALUE_UPDATE_DTO_ID_2 = "id2";
  private static final String ATTRIBUTE_VALUE_UPDATE_DTO_CODE_2 = "code2";
  private static final Integer ATTRIBUTE_VALUE_UPDATE_SEQUENCE_2 = 2;
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_2 = "value2";
  private static final String ATTRIBUTE_VALUE_UPDATE_DTO_ID_3 = "id3";
  private static final Integer ATTRIBUTE_VALUE_UPDATE_SEQUENCE_3 = 3;
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_3 = "value3";
  private static final String ATTRIBUTE_VALUE_UPDATE_DTO_ID_DUPLICATE_3 = "id3_duplicate";
  private static final String ATTRIBUTE_VALUE_UPDATE_DTO_ID_4 = "id4";
  private static final String ATTRIBUTE_VALUE_UPDATE_DTO_ID_DUPLICATE_4 = "id4_duplicate";
  private static final Integer ATTRIBUTE_VALUE_UPDATE_SEQUENCE_4 = 4;
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_4 = "value4";
  private static final String ATTRIBUTE_VALUE_UPDATE_DTO_ID_5 = "id5";
  private static final String ATTRIBUTE_VALUE_UPDATE_DTO_CODE_5 = "code5";
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_5 = "value5";
  private static final String ID = "id";
  private static final String UPDATED_BY = "updated_by";
  private static final String SORT_DIRECTTION = "ASC";
  private static final String SORTED_BY = "name";
  private static final String EXISTING_ALLOWED_ATTRIBUTE_VALUE = "Attribute value already exist";
  private static final String ALLOWED_ATTRIBUTE_CODE = "ATT-CODE-1";
  private static final String DESCRIPTION = "description";
  private static final String DIMENSION_ID = "dimensionId";
  private static final String VALUE_TYPES_JSON = "[\"Value\"]";
  private static final boolean VARIANT_CREATION = true;
  private Boolean GET_ALL_VALUES = Boolean.FALSE;
  private static  final String DS_EXTRACTION = "DS_EXTRACTION";

  @Mock
  private AllowedAttributeValueServiceBean allowedAttributeValueService;

  @Mock
  private PredefinedAllowedAttributeValueServiceBean predefinedAllowedAttributeValueService;

  @Mock
  private AttributeRepository repository;

  @Mock
  private AttributeService attributeService;

  @Mock
  private CategoryAttributeRepository categoryAttributeRepository;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Captor
  private ArgumentCaptor<Attribute> attributeArgumentCaptor;

  @Captor
  private ArgumentCaptor<AllowedAttributeValue> allowedAttributeValueArgumentCaptor;

  @Captor
  private ArgumentCaptor<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValueArgumentCaptor;

  @Mock
  private DomainEventPublisherService domainEventPublisherServiceBean;

  @Mock
  private DimensionService dimensionService;

  @Mock
  private DimensionMappingService dimensionMappingService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ObjectMapper mapper;

  @Captor
  private ArgumentCaptor<AttributeDomainEventModel> attributeDomainEventModelArgumentCaptor;

  @InjectMocks
  private MasterAttributeServiceBean masterAttributeServiceBean;

  private AttributeFilter attributeFilter;

  private List<Object[]> attrList;

  private Page<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValuePage;
  private Page<AllowedAttributeValue> allowedAttributeValuePage;
  private List<AllowedAttributeValue> allowedAttributeValueList;
  private AllowedAttributeValue allowedAttributeValue;
  private Page<AttributeValueDTO> attributeValueDTOPage;
  private List<AttributeValueDTO> attributeValueDTOList;
  private List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValueList;
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValue;
  private AttributeValueDTO attributeValue;
  private Attribute attribute;
  private MasterAttributeUpdateDTO masterAttributeUpdateDTO;
  private AllowedAttributeValue allowedAttributeValue1;
  private AllowedAttributeValue allowedAttributeValue2;
  private AllowedAttributeValue allowedAttributeValue3;
  private AllowedAttributeValue allowedAttributeValueDuplicate3;
  private AllowedAttributeValue allowedAttributeValue4;
  private AllowedAttributeValue allowedAttributeValueDuplicate4;
  private AllowedAttributeValue allowedAttributeValue5;
  private AttributeValueUpdateDTO attributeValueUpdateDTO1;
  private AttributeValueUpdateDTO attributeValueUpdateDTO2;
  private AttributeValueUpdateDTO attributeValueUpdateDTO3;
  private AttributeValueUpdateDTO attributeValueUpdateDTO4;
  private AttributeValueUpdateDTO attributeValueUpdateDTO5;
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValue1;
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValue2;
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValue3;
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValueDuplicate3;
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValue4;
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValueDuplicate4;
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValue5;
  private ObjectMapper objectMapper;
  private Date updatedDate;

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
    objectMapper = new ObjectMapper();
    MDC.put("storeId", "10001");
    attributeFilter = AttributeFilter.builder().attributeType(AttributeType.DEFINING_ATTRIBUTE.name())
        .name(ATTRIBUTE1_NAME).sortDirection(SORT_DIRECTTION).sortedBy(SORTED_BY).build();
    attrList = new ArrayList<Object[]>();
    attrList.add(new Object[]{"AS-1000018", "Category", "MA-0000027", "DESCRIPTIVE_ATTRIBUTE", "Manufacturer", "", ""});
    attrList.add(new Object[]{"AS-1000018", "Category", "OS-M000171", "PREDEFINED_ATTRIBUTE", "OS", "", "Android"});
    attrList.add(new Object[]{"AS-1000018", "Category", "OS-M000171", "PREDEFINED_ATTRIBUTE", "OS", "", "IOS"});
    attrList.add(new Object[]{"AS-1000018", "Category", "OS-M000171", "PREDEFINED_ATTRIBUTE", "OS", "", "Tizen"});
    attrList.add(new Object[]{"AS-1000018", "Category", "OU-2000004", "DEFINING_ATTRIBUTE", "Outlet", "BSD - Teras Kota", ""});
    attrList.add(new Object[]{"AS-1000018", "Category", "OU-2000004", "DEFINING_ATTRIBUTE", "Outlet", "BSD - AEON", ""});
    attrList.add(new Object[]{"AS-1000018", "Category", "OU-2000004", "DEFINING_ATTRIBUTE", "Outlet", "Bali - Lippo Plaza", ""});
    attrList.add(new Object[]{"AS-1000018", "Category", "OU-2000004", "DEFINING_ATTRIBUTE", "Outlet", "Bandung - Paris Van Java", ""});

    attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValue.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_1);
    predefinedAllowedAttributeValue.setSequence(SEQUENCE);
    predefinedAllowedAttributeValueList = new ArrayList<>();
    predefinedAllowedAttributeValueList.add(predefinedAllowedAttributeValue);
    predefinedAllowedAttributeValuePage =
        new PageImpl<>(predefinedAllowedAttributeValueList);
    attributeValue = new AttributeValueDTO(ATTRIBUTE_CODE, VALUE, SEQUENCE, null, null);
    attributeValueDTOList = new ArrayList<>();
    attributeValueDTOList.add(attributeValue);
    attributeValueDTOPage = new PageImpl<>(attributeValueDTOList);
    allowedAttributeValue = new AllowedAttributeValue();
    allowedAttributeValue.setAllowedAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValue.setSequence(SEQUENCE);
    allowedAttributeValue.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_1);
    allowedAttributeValueList = new ArrayList<>();
    allowedAttributeValueList.add(allowedAttributeValue);
    allowedAttributeValuePage = new PageImpl<>(allowedAttributeValueList);

    masterAttributeUpdateDTO = new MasterAttributeUpdateDTO();
    masterAttributeUpdateDTO.setUpdatedBy(USER_NAME);
    masterAttributeUpdateDTO.setUpdatedDate(new Date());
    masterAttributeUpdateDTO.setSortType(AttributeSortType.MANUAL);
    allowedAttributeValue1 = new AllowedAttributeValue();
    allowedAttributeValue1.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_1);
    allowedAttributeValue1.setValue("value1");
    allowedAttributeValue2 = new AllowedAttributeValue();
    allowedAttributeValue2.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_2);
    allowedAttributeValue2.setValue("value2");
    allowedAttributeValue3 = new AllowedAttributeValue();
    allowedAttributeValue3.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_3);
    allowedAttributeValue3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    allowedAttributeValueDuplicate3 = new AllowedAttributeValue();
    allowedAttributeValueDuplicate3.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_DUPLICATE_3);
    allowedAttributeValueDuplicate3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    allowedAttributeValue4 = new AllowedAttributeValue();
    allowedAttributeValue4.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_4);
    allowedAttributeValue4.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_4);
    allowedAttributeValueDuplicate4 = new AllowedAttributeValue();
    allowedAttributeValueDuplicate4.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_DUPLICATE_4);
    allowedAttributeValueDuplicate4.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_4);
    allowedAttributeValue5 = new AllowedAttributeValue();
    allowedAttributeValue5.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_5);
    allowedAttributeValue5.setMarkForDelete(false);
    allowedAttributeValue5.setValue("value5");
    predefinedAllowedAttributeValue1 = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue1.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_1);
    predefinedAllowedAttributeValue1.setValue("value1");
    predefinedAllowedAttributeValue2 = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue2.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_2);
    predefinedAllowedAttributeValue2.setValue("value2");
    predefinedAllowedAttributeValue3 = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue3.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_3);
    predefinedAllowedAttributeValue3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    predefinedAllowedAttributeValueDuplicate3 = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValueDuplicate3.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_DUPLICATE_3);
    predefinedAllowedAttributeValueDuplicate3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    predefinedAllowedAttributeValue4 = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue4.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_4);
    predefinedAllowedAttributeValue4.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_4);
    predefinedAllowedAttributeValueDuplicate4 = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValueDuplicate4.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_DUPLICATE_4);
    predefinedAllowedAttributeValueDuplicate4.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_4);
    predefinedAllowedAttributeValue5 = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue5.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_5);
    predefinedAllowedAttributeValue5.setMarkForDelete(false);
    predefinedAllowedAttributeValue5.setValue("value5");
    attributeValueUpdateDTO1 = new AttributeValueUpdateDTO();
    attributeValueUpdateDTO1.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_1);
    attributeValueUpdateDTO1.setAllowedAttributeCode(ATTRIBUTE_VALUE_UPDATE_DTO_CODE_1);
    attributeValueUpdateDTO1.setSequence(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_1);
    attributeValueUpdateDTO1.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_1);
    attributeValueUpdateDTO2 = new AttributeValueUpdateDTO();
    attributeValueUpdateDTO2.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_2);
    attributeValueUpdateDTO2.setAllowedAttributeCode(ATTRIBUTE_VALUE_UPDATE_DTO_CODE_2);
    attributeValueUpdateDTO2.setSequence(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_2);
    attributeValueUpdateDTO2.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_2);
    attributeValueUpdateDTO3 = new AttributeValueUpdateDTO();
    attributeValueUpdateDTO3.setSequence(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_3);
    attributeValueUpdateDTO3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    attributeValueUpdateDTO4 = new AttributeValueUpdateDTO();
    attributeValueUpdateDTO4.setSequence(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_4);
    attributeValueUpdateDTO4.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_4);
    attributeValueUpdateDTO5 = new AttributeValueUpdateDTO();
    attributeValueUpdateDTO5.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_5);
    attributeValueUpdateDTO5.setAllowedAttributeCode(ATTRIBUTE_VALUE_UPDATE_DTO_CODE_5);
    attributeValueUpdateDTO5.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_5);
    updatedDate = new Date();
  }

  @AfterEach
  public void postTest() {
    verifyNoMoreInteractions(this.repository);
    verifyNoMoreInteractions(this.allowedAttributeValueService);
    verifyNoMoreInteractions(this.predefinedAllowedAttributeValueService);
    verifyNoMoreInteractions(this.attributeService);
    verifyNoMoreInteractions(this.applicationCacheServiceBean);
    verifyNoMoreInteractions(this.dimensionMappingService);
    verifyNoMoreInteractions(this.dimensionService);
    verifyNoMoreInteractions(this.systemParameterService);
    verifyNoMoreInteractions(this.mapper);
  }

  @Test
  public void getAttributeValuesByAttributeCodeForPredefinedAttributesTest() throws Exception {
    AttributeTypeDTO attributeTypeDTO = new AttributeTypeDTO();
    attributeTypeDTO.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeTypeDTO.setAttributeCode(ATTRIBUTE_CODE);
    attributeTypeDTO.setAttributeId(ATTRIBUTE_ID);
    attributeTypeDTO.setAttributeSortType(AttributeSortType.ALPHABETICAL);
    Mockito.when(this.repository.getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attributeTypeDTO);
    Mockito.when(this.predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST,
            attributeTypeDTO.getAttributeSortType())).thenReturn(predefinedAllowedAttributeValuePage);
    Page<AttributeValueDTO> attributeValueDTOS = this.masterAttributeServiceBean
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, DEFAULT_PAGE_REQUEST, GET_ALL_VALUES, false);
    Mockito.verify(repository).getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    Mockito.verify(predefinedAllowedAttributeValueService)
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST,
            attributeTypeDTO.getAttributeSortType());
    assertTrue(attributeValueDTOS.getContent().size() == 1);
    Assertions.assertTrue(attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode().equals(ATTRIBUTE_CODE));
    assertTrue(StringUtils.isEmpty(attributeValueDTOS.getContent().get(0).getAllowedAttributeCode()));
  }

  @Test
  public void getAttributeValuesByAttributeCodeForPredefinedMutliValueAttributesTest() throws Exception {
    AttributeTypeDTO attributeTypeDTO = new AttributeTypeDTO();
    attributeTypeDTO.setAttributeType(AttributeType.PREDEFINED_MULTIVALUE);
    attributeTypeDTO.setAttributeCode(ATTRIBUTE_CODE);
    attributeTypeDTO.setAttributeId(ATTRIBUTE_ID);
    attributeTypeDTO.setAttributeSortType(AttributeSortType.ALPHABETICAL);
    Mockito.when(this.repository.getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attributeTypeDTO);
    Mockito.when(this.predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST,
            attributeTypeDTO.getAttributeSortType())).thenReturn(predefinedAllowedAttributeValuePage);
    Page<AttributeValueDTO> attributeValueDTOS = this.masterAttributeServiceBean
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, DEFAULT_PAGE_REQUEST, GET_ALL_VALUES, false);
    Mockito.verify(repository).getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    Mockito.verify(predefinedAllowedAttributeValueService)
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST,
            attributeTypeDTO.getAttributeSortType());
    assertTrue(attributeValueDTOS.getContent().size() == 1);
    Assertions.assertTrue(attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode().equals(ATTRIBUTE_CODE));
    assertTrue(StringUtils.isEmpty(attributeValueDTOS.getContent().get(0).getAllowedAttributeCode()));
  }

  @Test
  public void getAttributeValuesByAttributeCodeForDefiningAttributesTest() throws Exception {
    AttributeTypeDTO attributeTypeDTO = new AttributeTypeDTO();
    attributeTypeDTO.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attributeTypeDTO.setAttributeCode(ATTRIBUTE_CODE);
    attributeTypeDTO.setAttributeId(ATTRIBUTE_ID);
    attributeTypeDTO.setAttributeSortType(AttributeSortType.ALPHABETICAL);
    Mockito.when(this.repository.getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attributeTypeDTO);
    Mockito.when(this.allowedAttributeValueService
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST, attributeTypeDTO.getAttributeSortType()))
        .thenReturn(allowedAttributeValuePage);
    Page<AttributeValueDTO> attributeValueDTOS = this.masterAttributeServiceBean
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, DEFAULT_PAGE_REQUEST, GET_ALL_VALUES, false);
    Mockito.verify(repository).getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    Mockito.verify(allowedAttributeValueService)
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST, attributeTypeDTO.getAttributeSortType());
    assertTrue(attributeValueDTOS.getContent().size() == 1);
    assertTrue(attributeValueDTOS.getContent().get(0).getAllowedAttributeCode().equals(ATTRIBUTE_CODE));
    assertTrue(StringUtils.isEmpty(attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode()));
    MDC.put("storeId", "10001");
  }

  @Test
  public void getAttributeValuesByAttributeCodeForPredefinedAttributesTest_getAllValues() throws Exception {
    attribute.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValueList);
    AttributeTypeDTO attributeTypeDTO = new AttributeTypeDTO();
    attributeTypeDTO.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeTypeDTO.setAttributeCode(ATTRIBUTE_CODE);
    attributeTypeDTO.setAttributeId(ATTRIBUTE_ID);
    attributeTypeDTO.setAttributeSortType(AttributeSortType.ALPHABETICAL);
    GET_ALL_VALUES = Boolean.TRUE;
    Mockito.when(this.repository.getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attributeTypeDTO);
    Mockito.when(this.attributeService.findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false)).thenReturn(attribute);
    Page<AttributeValueDTO> attributeValueDTOS = this.masterAttributeServiceBean
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, DEFAULT_PAGE_REQUEST, GET_ALL_VALUES, false);
    Mockito.verify(this.attributeService).findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false);
    Mockito.verify(repository).getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    assertTrue(attributeValueDTOS.getContent().size() == 1);
    Assertions.assertTrue(attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode().equals(ATTRIBUTE_CODE));
    assertTrue(StringUtils.isEmpty(attributeValueDTOS.getContent().get(0).getAllowedAttributeCode()));
  }

  @Test
  public void getAttributeValuesByAttributeCodeForPredefinedAttributesTestForDefaultValue() throws Exception {
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue
        .setPredefinedAllowedAttributeCode(attribute.getAttributeCode() + Constants.HYPHEN + Constants.DEFAULT);
    predefinedAllowedAttributeValueList.add(predefinedAllowedAttributeValue);
    attribute.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValueList);
    predefinedAllowedAttributeValuePage =
        new PageImpl<>(predefinedAllowedAttributeValueList);
    AttributeTypeDTO attributeTypeDTO = new AttributeTypeDTO();
    attributeTypeDTO.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeTypeDTO.setAttributeCode(ATTRIBUTE_CODE);
    attributeTypeDTO.setAttributeId(ATTRIBUTE_ID);
    attributeTypeDTO.setAttributeSortType(AttributeSortType.ALPHABETICAL);
    attributeTypeDTO.setMandatory(true);
    Mockito.when(this.repository.getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attributeTypeDTO);
    Mockito.when(this.predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST,
            attributeTypeDTO.getAttributeSortType())).thenReturn(predefinedAllowedAttributeValuePage);
    Page<AttributeValueDTO> attributeValueDTOS = this.masterAttributeServiceBean
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, DEFAULT_PAGE_REQUEST, GET_ALL_VALUES, false);
    Mockito.verify(repository).getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    Mockito.verify(predefinedAllowedAttributeValueService)
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST,
            attributeTypeDTO.getAttributeSortType());
    assertEquals(1, attributeValueDTOS.getContent().size());
    Assertions.assertTrue(attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode().equals(ATTRIBUTE_CODE));
    assertEquals(ATTRIBUTE_CODE, attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode());
  }

  @Test
  public void getAttributeValuesByAttributeCodeForPredefinedAttributesTest_getAllValues_removeMFDTrueObjects() throws Exception {
    predefinedAllowedAttributeValue1.setMarkForDelete(Boolean.TRUE);
    predefinedAllowedAttributeValue1.setValue(VALUE);
    predefinedAllowedAttributeValueList.add(predefinedAllowedAttributeValue1);
    attribute.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValueList);
    AttributeTypeDTO attributeTypeDTO = new AttributeTypeDTO();
    attributeTypeDTO.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeTypeDTO.setAttributeCode(ATTRIBUTE_CODE);
    attributeTypeDTO.setAttributeId(ATTRIBUTE_ID);
    attributeTypeDTO.setAttributeSortType(AttributeSortType.ALPHABETICAL);
    GET_ALL_VALUES = Boolean.TRUE;
    Mockito.when(this.repository.getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attributeTypeDTO);
    Mockito.when(this.attributeService.findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false)).thenReturn(attribute);
    Page<AttributeValueDTO> attributeValueDTOS = this.masterAttributeServiceBean
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, DEFAULT_PAGE_REQUEST, GET_ALL_VALUES, false);
    Mockito.verify(this.attributeService).findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false);
    Mockito.verify(repository).getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    assertTrue(attributeValueDTOS.getContent().size() == 1);
    Assertions.assertTrue(attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode().equals(ATTRIBUTE_CODE));
    assertTrue(StringUtils.isEmpty(attributeValueDTOS.getContent().get(0).getAllowedAttributeCode()));
  }

  @Test
  public void getAttributeValuesByAttributeCodeForDefiningAttributesTest_getAllValues() throws Exception {
    GET_ALL_VALUES = Boolean.TRUE;
    attribute.setAllowedAttributeValues(allowedAttributeValueList);
    AttributeTypeDTO attributeTypeDTO = new AttributeTypeDTO();
    attributeTypeDTO.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attributeTypeDTO.setAttributeCode(ATTRIBUTE_CODE);
    attributeTypeDTO.setAttributeId(ATTRIBUTE_ID);
    attributeTypeDTO.setAttributeSortType(AttributeSortType.ALPHABETICAL);
    Mockito.when(this.repository.getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attributeTypeDTO);
    Mockito.when(this.attributeService.findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false)).thenReturn(attribute);
    Page<AttributeValueDTO> attributeValueDTOS = this.masterAttributeServiceBean
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, DEFAULT_PAGE_REQUEST, GET_ALL_VALUES, false);
    Mockito.verify(repository).getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    Mockito.verify(this.attributeService).findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false);
    assertTrue(attributeValueDTOS.getContent().size() == 1);
    assertTrue(attributeValueDTOS.getContent().get(0).getAllowedAttributeCode().equals(ATTRIBUTE_CODE));
    assertTrue(StringUtils.isEmpty(attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode()));
  }

  @Test
  public void getAttributeValuesByAttributeCodeConcatenateValueWithValueTypeTrueTest() throws Exception {
    ReflectionTestUtils.setField(masterAttributeServiceBean, "valueTypeAdditionForDefiningAttributes", true);
    ReflectionTestUtils.setField(masterAttributeServiceBean, "sizeChartValueTypeDelimiter", " \u200C- ");
    allowedAttributeValue.setValueType(VALUE_TYPES_JSON);
    allowedAttributeValue.setValue(VALUE);
    GET_ALL_VALUES = Boolean.TRUE;
    attribute.setAllowedAttributeValues(allowedAttributeValueList);
    AttributeTypeDTO attributeTypeDTO = new AttributeTypeDTO();
    attributeTypeDTO.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attributeTypeDTO.setAttributeCode(ATTRIBUTE_CODE);
    attributeTypeDTO.setAttributeId(ATTRIBUTE_ID);
    attributeTypeDTO.setAttributeSortType(AttributeSortType.ALPHABETICAL);
    Mockito.when(this.repository.getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attributeTypeDTO);
    Mockito.when(this.attributeService.findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false)).thenReturn(attribute);
    Page<AttributeValueDTO> attributeValueDTOS =
        this.masterAttributeServiceBean.getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE,
            DEFAULT_PAGE_REQUEST, GET_ALL_VALUES, true);
    Mockito.verify(repository).getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    Mockito.verify(this.attributeService).findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false);
    assertEquals(1, attributeValueDTOS.getContent().size());
    assertEquals(ATTRIBUTE_CODE, attributeValueDTOS.getContent().get(0).getAllowedAttributeCode());
    assertTrue(StringUtils.isEmpty(attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode()));
  }

  @Test
  public void getAttributeValuesByAttributeCodeForDefiningAttributesTest_getAllValues_removeMFDTrueObjects()
      throws Exception {
    GET_ALL_VALUES = Boolean.TRUE;
    allowedAttributeValue1.setValue(VALUE);
    allowedAttributeValue1.setMarkForDelete(Boolean.TRUE);
    allowedAttributeValueList.add(allowedAttributeValue1);
    attribute.setAllowedAttributeValues(allowedAttributeValueList);
    AttributeTypeDTO attributeTypeDTO = new AttributeTypeDTO();
    attributeTypeDTO.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attributeTypeDTO.setAttributeCode(ATTRIBUTE_CODE);
    attributeTypeDTO.setAttributeId(ATTRIBUTE_ID);
    attributeTypeDTO.setAttributeSortType(AttributeSortType.ALPHABETICAL);
    Mockito.when(this.repository.getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attributeTypeDTO);
    Mockito.when(this.attributeService.findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false)).thenReturn(attribute);
    Page<AttributeValueDTO> attributeValueDTOS = this.masterAttributeServiceBean
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, DEFAULT_PAGE_REQUEST, GET_ALL_VALUES, false);
    Mockito.verify(repository).getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    Mockito.verify(this.attributeService).findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false);
    assertTrue(attributeValueDTOS.getContent().size() == 1);
    assertTrue(attributeValueDTOS.getContent().get(0).getAllowedAttributeCode().equals(ATTRIBUTE_CODE));
    assertTrue(StringUtils.isEmpty(attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode()));
  }

  @Test
  public void getAttributeValuesByAttributeCodeForPredefinedAttributesTest_getAllValues_manualSortType()
      throws Exception {
    attribute.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValueList);
    AttributeTypeDTO attributeTypeDTO = new AttributeTypeDTO();
    attributeTypeDTO.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeTypeDTO.setAttributeCode(ATTRIBUTE_CODE);
    attributeTypeDTO.setAttributeId(ATTRIBUTE_ID);
    attributeTypeDTO.setAttributeSortType(AttributeSortType.MANUAL);
    GET_ALL_VALUES = Boolean.TRUE;
    Mockito.when(this.repository.getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attributeTypeDTO);
    Mockito.when(this.attributeService.findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false)).thenReturn(attribute);
    Page<AttributeValueDTO> attributeValueDTOS = this.masterAttributeServiceBean
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, DEFAULT_PAGE_REQUEST, GET_ALL_VALUES, false);
    Mockito.verify(this.attributeService).findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false);
    Mockito.verify(repository).getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    assertTrue(attributeValueDTOS.getContent().size() == 1);
    Assertions.assertTrue(attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode().equals(ATTRIBUTE_CODE));
    assertTrue(StringUtils.isEmpty(attributeValueDTOS.getContent().get(0).getAllowedAttributeCode()));
  }

  @Test
  public void getAttributeValuesByAttributeCodeForPredefinedAttributesTestGetAllValuesCustomSortType()
      throws Exception {
    attribute.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValueList);
    AttributeTypeDTO attributeTypeDTO = new AttributeTypeDTO();
    attributeTypeDTO.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeTypeDTO.setAttributeCode(ATTRIBUTE_CODE);
    attributeTypeDTO.setAttributeId(ATTRIBUTE_ID);
    attributeTypeDTO.setAttributeSortType(AttributeSortType.CUSTOM);
    GET_ALL_VALUES = Boolean.TRUE;
    Mockito.when(this.repository.getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attributeTypeDTO);
    Mockito.when(this.attributeService.findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false)).thenReturn(attribute);
    Page<AttributeValueDTO> attributeValueDTOS = this.masterAttributeServiceBean
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, DEFAULT_PAGE_REQUEST, GET_ALL_VALUES, false);
    Mockito.verify(this.attributeService).findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false);
    Mockito.verify(repository).getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    assertTrue(attributeValueDTOS.getContent().size() == 1);
    Assertions.assertTrue(attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode().equals(ATTRIBUTE_CODE));
    assertTrue(StringUtils.isEmpty(attributeValueDTOS.getContent().get(0).getAllowedAttributeCode()));
  }

  @Test
  public void getAttributeValuesByAttributeCodeForDefiningAttributesTest_getAllValues_manualSortType()
      throws Exception {
    GET_ALL_VALUES = Boolean.TRUE;
    attribute.setAllowedAttributeValues(allowedAttributeValueList);
    AttributeTypeDTO attributeTypeDTO = new AttributeTypeDTO();
    attributeTypeDTO.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attributeTypeDTO.setAttributeCode(ATTRIBUTE_CODE);
    attributeTypeDTO.setAttributeId(ATTRIBUTE_ID);
    attributeTypeDTO.setAttributeSortType(AttributeSortType.MANUAL);
    Mockito.when(this.repository.getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attributeTypeDTO);
    Mockito.when(this.attributeService.findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false)).thenReturn(attribute);
    Page<AttributeValueDTO> attributeValueDTOS = this.masterAttributeServiceBean
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, DEFAULT_PAGE_REQUEST, GET_ALL_VALUES, false);
    Mockito.verify(repository).getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    Mockito.verify(this.attributeService).findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false);
    assertTrue(attributeValueDTOS.getContent().size() == 1);
    assertTrue(attributeValueDTOS.getContent().get(0).getAllowedAttributeCode().equals(ATTRIBUTE_CODE));
    assertTrue(StringUtils.isEmpty(attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode()));
  }

  @Test
  public void getAttributeValuesByAttributeCodeForDefiningAttributesTestGetAllValuesCustomSortType()
      throws Exception {
    GET_ALL_VALUES = Boolean.TRUE;
    attribute.setAllowedAttributeValues(allowedAttributeValueList);
    AttributeTypeDTO attributeTypeDTO = new AttributeTypeDTO();
    attributeTypeDTO.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attributeTypeDTO.setAttributeCode(ATTRIBUTE_CODE);
    attributeTypeDTO.setAttributeId(ATTRIBUTE_ID);
    attributeTypeDTO.setAttributeSortType(AttributeSortType.CUSTOM);
    Mockito.when(this.repository.getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attributeTypeDTO);
    Mockito.when(this.attributeService.findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false)).thenReturn(attribute);
    Page<AttributeValueDTO> attributeValueDTOS = this.masterAttributeServiceBean
        .getAttributeValuesByAttributeCode(STORE_ID, ATTRIBUTE_CODE, DEFAULT_PAGE_REQUEST, GET_ALL_VALUES, false);
    Mockito.verify(repository).getAttributeTypeInfoByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    Mockito.verify(this.attributeService).findDetailByStoreIdAndId(STORE_ID, ATTRIBUTE_ID, false);
    assertTrue(attributeValueDTOS.getContent().size() == 1);
    assertTrue(attributeValueDTOS.getContent().get(0).getAllowedAttributeCode().equals(ATTRIBUTE_CODE));
    assertTrue(StringUtils.isEmpty(attributeValueDTOS.getContent().get(0).getPredefinedAllowedAttributeCode()));
  }

  @Test
  public void findDetailByAttributeCodeTest() {
    attribute.setDsExtraction(true);
    Mockito.when(this.repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    Attribute result = this.masterAttributeServiceBean.findDetailByAttributeCode(ATTRIBUTE_CODE);
    Mockito.verify(this.repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    assertEquals(ATTRIBUTE_CODE, result.getAttributeCode());
    assertTrue(result.isDsExtraction());
  }

  @Test
  public void findDetailByAttributeCode_AttributeInfoNotFoundTest() {
    try {
      Mockito.when(this.repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(null);
      this.masterAttributeServiceBean.findDetailByAttributeCode(ATTRIBUTE_CODE);
    } catch (Exception e) {
      assertTrue(e.getMessage().contains(NOT_FOUND_ATTRIBUTE_WITH_ATTRIBUTE_CODE));
      Mockito.verify(this.repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    }
  }

  private Attribute getDefaultAttribute() {
    return new Attribute(ATTRIBUTE1_NAME, AttributeType.DEFINING_ATTRIBUTE, false, STORE_ID);
  }

  private AllowedAttributeValue getDefaultAllowedAttributeValue() {
    return new AllowedAttributeValue(this.getDefaultAttribute(), VALUE, STORE_ID, 1);
  }

  private PredefinedAllowedAttributeValue getDefaultPredefinedAllowedAttributeValue() {
    return new PredefinedAllowedAttributeValue(this.getDefaultAttribute(), VALUE, STORE_ID, 1);
  }

  private Attribute setUpAttribute(){
    Attribute attribute = new Attribute();
    this.getDefaultAttribute();
    List<AllowedAttributeValue> allowedAttributeValues = new ArrayList<>();
    allowedAttributeValues.add(this.getDefaultAllowedAttributeValue());
    attribute.setAllowedAttributeValues(allowedAttributeValues);
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = new ArrayList<>();
    predefinedAllowedAttributeValues.add(this.getDefaultPredefinedAllowedAttributeValue());
    attribute.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValues);
    return attribute;
  }

  @Test
  public void testInsertMasterAttribute_DsExtractionTrue() throws Exception {
    Attribute attribute = setUpAttribute();
    List<DimensionMappingRequest> dimensionMappingRequestList = new ArrayList<>();
    List<String> valueTypeList = new ArrayList<>();
    attribute.setStoreId(STORE_ID);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setDsExtraction(true);
    attribute.setHideForCustomer(true);
    attribute.setHideForSeller(true);
    attribute.setDsAttributeName(DS_ATTRIBUTE_NAME);
    String uuid = GdnUUIDHelper.generateUUID();
    when(attributeService.save(attribute)).thenReturn(uuid);
    when(repository.findByStoreIdAndDsAttributeNameAndMarkForDeleteFalse(STORE_ID,
        DS_ATTRIBUTE_NAME)).thenReturn(null);
    Assertions.assertEquals(
        masterAttributeServiceBean.insertMasterAttribute(attribute, dimensionMappingRequestList,
            valueTypeList), (uuid));
    verify(attributeService, AT_LEAST_ONCE).save(attribute);
    verify(repository).findByStoreIdAndDsAttributeNameAndMarkForDeleteFalse(STORE_ID,
        DS_ATTRIBUTE_NAME);
    assertNull(attribute.getDescriptionSearch());
  }

  @Test
  public void testInsertMasterAttribute_DsExtractionTrueAndDuplicateDSAttributeName() throws Exception {
    Attribute attribute = setUpAttribute();
    List<DimensionMappingRequest> dimensionMappingRequestList = new ArrayList<>();
    List<String> valueTypeList = new ArrayList<>();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setDsExtraction(true);
    attribute.setHideForCustomer(true);
    attribute.setHideForSeller(true);
    attribute.setStoreId(STORE_ID);
    attribute.setDsAttributeName(DS_ATTRIBUTE_NAME);
    String uuid = GdnUUIDHelper.generateUUID();
    when(attributeService.save(attribute)).thenReturn(uuid);
    when(repository.findByStoreIdAndDsAttributeNameAndMarkForDeleteFalse(STORE_ID,
        DS_ATTRIBUTE_NAME)).thenReturn(new Attribute());
    try {
      masterAttributeServiceBean.insertMasterAttribute(attribute, dimensionMappingRequestList,
          valueTypeList);
    } catch (ValidationException ex){
      Assertions.assertEquals(ErrorMessage.DUPLICATE_DS_ATTRIBUTE_NAME_ERROR_CODE.getMessage(),
          ex.getErrorCode());
    }
    verify(repository).findByStoreIdAndDsAttributeNameAndMarkForDeleteFalse(STORE_ID,
        DS_ATTRIBUTE_NAME);
  }

  @Test
  public void testInsertMasterSizeAttribute() throws Exception {
    Attribute attribute = setUpAttribute();
    attribute.setSizeAttribute(true);
    attribute.setValueTypeAttribute(true);
    attribute.setDsExtraction(false);
    attribute.setHideForCustomer(true);
    attribute.setHideForSeller(true);
    DimensionMappingRequest dimensionMappingRequest =
        new DimensionMappingRequest(true, DIMENSION_ID);
    List<DimensionMappingRequest> dimensionMappingRequestList = new ArrayList<>();
    dimensionMappingRequestList.add(dimensionMappingRequest);
    List<String> valueTypeList = new ArrayList<>();
    valueTypeList.add(VALUE);
    SystemParameter systemParameter =
        new SystemParameter(Constants.VALUE_TYPES_CONFIG, VALUE, DESCRIPTION);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    String uuid = GdnUUIDHelper.generateUUID();
    when(attributeService.save(attribute)).thenReturn(uuid);
    when(systemParameterService.findByStoreIdAndVariable(attribute.getStoreId(),
        Constants.VALUE_TYPES_CONFIG)).thenReturn(systemParameter);
    when(dimensionService.findById(attribute.getStoreId(), DIMENSION_ID)).thenReturn(
        new Dimension());
    Assertions.assertEquals(
        masterAttributeServiceBean.insertMasterAttribute(attribute, dimensionMappingRequestList,
            valueTypeList), (uuid));
    verify(attributeService, AT_LEAST_ONCE).save(attribute);
    verify(dimensionService).findById(attribute.getStoreId(), DIMENSION_ID);
    verify(dimensionMappingService).save(new DimensionMapping());
    verify(systemParameterService).findByStoreIdAndVariable(attribute.getStoreId(),
        Constants.VALUE_TYPES_CONFIG);
    verify(mapper).writeValueAsString(valueTypeList);
    assertNull(attribute.getDescriptionSearch());
  }

  @Test
  public void testInsertMasterAttributeNotNullDescription() throws Exception {
    Attribute attribute = setUpAttribute();
    List<DimensionMappingRequest> dimensionMappingRequestList = new ArrayList<>();
    List<String> valueTypeList = new ArrayList<>();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setDescription(DESCRIPTION.getBytes());
    attribute.setDsExtraction(false);
    attribute.setHideForCustomer(true);
    attribute.setHideForSeller(true);
    String uuid = GdnUUIDHelper.generateUUID();
    when(attributeService.save(attribute)).thenReturn(uuid);
    Assertions.assertEquals(
        masterAttributeServiceBean.insertMasterAttribute(attribute, dimensionMappingRequestList,
            valueTypeList), (uuid));
    verify(attributeService, AT_LEAST_ONCE).save(attribute);
    assertEquals(new String(attribute.getDescription()), attribute.getDescriptionSearch());
  }

  @Test
  public void testInsertMasterAttributeEmptyRequest() throws Exception {
    Attribute attribute = setUpAttribute();
    attribute.setSizeAttribute(true);
    List<DimensionMappingRequest> dimensionMappingRequestList = new ArrayList<>();
    List<String> valueTypeList = new ArrayList<>();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setDescription(DESCRIPTION.getBytes());
    attribute.setDsExtraction(false);
    attribute.setHideForCustomer(true);
    attribute.setHideForSeller(true);
    String uuid = GdnUUIDHelper.generateUUID();
    when(attributeService.save(attribute)).thenReturn(uuid);
    try {
      masterAttributeServiceBean.insertMasterAttribute(attribute, dimensionMappingRequestList,
          valueTypeList);
    } catch (ValidationException e) {

    } finally {
      verify(attributeService, AT_LEAST_ONCE).save(attribute);
      assertEquals(new String(attribute.getDescription()), attribute.getDescriptionSearch());
    }
  }

  @Test
  public void testInsertMasterAttributeEmptyDimensionId() throws Exception {
    Attribute attribute = setUpAttribute();
    attribute.setSizeAttribute(true);
    List<DimensionMappingRequest> dimensionMappingRequestList = new ArrayList<>();
    dimensionMappingRequestList.add(new DimensionMappingRequest());
    List<String> valueTypeList = new ArrayList<>();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setDescription(DESCRIPTION.getBytes());
    attribute.setDsExtraction(false);
    attribute.setHideForCustomer(false);
    attribute.setHideForSeller(true);
    String uuid = GdnUUIDHelper.generateUUID();
    when(attributeService.save(attribute)).thenReturn(uuid);
    try {
      masterAttributeServiceBean.insertMasterAttribute(attribute, dimensionMappingRequestList,
          valueTypeList);
    } catch (ValidationException e) {

    } finally {
      verify(attributeService, AT_LEAST_ONCE).save(attribute);
      assertEquals(new String(attribute.getDescription()), attribute.getDescriptionSearch());
    }
  }

  @Test
  public void updateAttributeValues_DefiningAttribute_Test() throws Exception {
    allowedAttributeValue3.setMarkForDelete(true);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setAllowedAttributeValues(
        new ArrayList<>(Arrays.asList(allowedAttributeValue1, allowedAttributeValue2, allowedAttributeValue5)));
    masterAttributeUpdateDTO.getAttributeValues().addAll(
        Arrays.asList(attributeValueUpdateDTO1, attributeValueUpdateDTO2));
    masterAttributeUpdateDTO.getAddedAttributeValues().addAll(
        Arrays.asList(attributeValueUpdateDTO3, attributeValueUpdateDTO4));
    masterAttributeUpdateDTO.getDeletedAttributeValues().add(attributeValueUpdateDTO5);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(attributeArgumentCaptor.capture());
    when(allowedAttributeValueService.save(any(AllowedAttributeValue.class))).thenReturn(ID);
    when(allowedAttributeValueService.getSequence(ATTRIBUTE_CODE)).thenReturn(ATTRIBUTE_CODE);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
    verify(allowedAttributeValueService, times(2)).getSequence(ATTRIBUTE_CODE);
    verify(allowedAttributeValueService, times(2)).save(allowedAttributeValueArgumentCaptor.capture());
    verify(attributeService).update(attributeArgumentCaptor.getValue());
    assertEquals(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_3,
        allowedAttributeValueArgumentCaptor.getAllValues().get(0).getSequence());
    assertEquals(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_4,
        allowedAttributeValueArgumentCaptor.getAllValues().get(1).getSequence());
    assertEquals(1, attributeArgumentCaptor.getValue().getAllowedAttributeValues().stream()
        .filter(attributeValue -> attributeValue.isMarkForDelete()).count());
    assertEquals(AttributeSortType.MANUAL, attributeArgumentCaptor.getValue().getSortType());
  }

  @Test
  public void updateAttributeValues_AddExistingDefiningAttribute_Test() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    allowedAttributeValue3.setMarkForDelete(false);
    allowedAttributeValue4.setMarkForDelete(false);
    allowedAttributeValueDuplicate3.setMarkForDelete(false);
    allowedAttributeValueDuplicate4.setMarkForDelete(true);
    attribute.setAllowedAttributeValues(
        new ArrayList<>(Arrays.asList(allowedAttributeValue3, allowedAttributeValue4, allowedAttributeValue5)));
    masterAttributeUpdateDTO.getAddedAttributeValues().addAll(
        Arrays.asList(attributeValueUpdateDTO3, attributeValueUpdateDTO4));
    masterAttributeUpdateDTO.getDeletedAttributeValues().add(attributeValueUpdateDTO5);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    try {
      masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO);
    }catch (ApplicationRuntimeException e){

    }finally {
      verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    }
  }

  @Test
  public void updateAttributeValues_DefiningAttribute_AttributeValueNotFound_Test() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    masterAttributeUpdateDTO.getAttributeValues().add(attributeValueUpdateDTO1);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO));
    } finally {
      verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    }
  }

  @Test
  public void updateAttributeValues_DefiningAttribute_AddAnExistingDeletedValue_Test() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    allowedAttributeValue3.setMarkForDelete(true);
    attribute.setAllowedAttributeValues(Arrays.asList(allowedAttributeValue3));
    AllowedAttributeValue allowedAttributeValue3 = new AllowedAttributeValue();
    allowedAttributeValue3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    allowedAttributeValue3.setMarkForDelete(true);
    masterAttributeUpdateDTO.getAddedAttributeValues().add(attributeValueUpdateDTO3);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(attributeArgumentCaptor.capture());
    when(domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.capture())).thenReturn(new AttributeDomainEventModel());
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(domainEventPublisherServiceBean)
        .publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.getValue());
    assertTrue(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    assertFalse(attributeArgumentCaptor.getValue().getAllowedAttributeValues().get(0).isMarkForDelete());
    verify(attributeService).update(attributeArgumentCaptor.getValue());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValues_DefiningAttributeAddAnExistingDeletedValueManualSortTest() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setSortType(AttributeSortType.MANUAL);
    allowedAttributeValue3.setMarkForDelete(true);
    attribute.setAllowedAttributeValues(Arrays.asList(allowedAttributeValue3));
    AllowedAttributeValue allowedAttributeValue3 = new AllowedAttributeValue();
    allowedAttributeValue3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    allowedAttributeValue3.setMarkForDelete(true);
    masterAttributeUpdateDTO.getAddedAttributeValues().add(attributeValueUpdateDTO3);
    masterAttributeUpdateDTO.setSortType(AttributeSortType.CUSTOM);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(attributeArgumentCaptor.capture());
    when(domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.capture())).thenReturn(new AttributeDomainEventModel());
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(domainEventPublisherServiceBean)
        .publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.getValue());
    assertTrue(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    assertFalse(attributeArgumentCaptor.getValue().getAllowedAttributeValues().get(0).isMarkForDelete());
    verify(attributeService).update(attributeArgumentCaptor.getValue());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValuesDefiningAttributeAddAnExistingDeletedValueAlphabeticalSortTest() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setSortType(AttributeSortType.MANUAL);
    allowedAttributeValue3.setMarkForDelete(true);
    attribute.setAllowedAttributeValues(Arrays.asList(allowedAttributeValue3));
    AllowedAttributeValue allowedAttributeValue3 = new AllowedAttributeValue();
    allowedAttributeValue3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    allowedAttributeValue3.setMarkForDelete(true);
    masterAttributeUpdateDTO.getAddedAttributeValues().add(attributeValueUpdateDTO3);
    masterAttributeUpdateDTO.setSortType(AttributeSortType.CUSTOM);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(attributeArgumentCaptor.capture());
    when(domainEventPublisherServiceBean
        .publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.capture()))
        .thenReturn(new AttributeDomainEventModel());
    masterAttributeUpdateDTO.setSortType(AttributeSortType.ALPHABETICAL);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(domainEventPublisherServiceBean)
        .publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.getValue());
    assertTrue(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    assertFalse(attributeArgumentCaptor.getValue().getAllowedAttributeValues().get(0).isMarkForDelete());
    verify(attributeService).update(attributeArgumentCaptor.getValue());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValuesDefiningAttributeAddSizeAttributeTest() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setSortType(AttributeSortType.MANUAL);
    attribute.setSizeAttribute(true);
    allowedAttributeValue3.setMarkForDelete(true);
    attribute.setAllowedAttributeValues(Arrays.asList(allowedAttributeValue3));
    AllowedAttributeValue allowedAttributeValue3 = new AllowedAttributeValue();
    allowedAttributeValue3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    allowedAttributeValue3.setMarkForDelete(true);
    masterAttributeUpdateDTO.getAddedAttributeValues().add(attributeValueUpdateDTO3);
    masterAttributeUpdateDTO.setSortType(AttributeSortType.CUSTOM);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(attributeArgumentCaptor.capture());
    when(domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.capture())).thenReturn(
        new AttributeDomainEventModel());
    masterAttributeUpdateDTO.setSortType(AttributeSortType.ALPHABETICAL);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE,
        masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(domainEventPublisherServiceBean).publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.getValue());
    assertTrue(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    assertFalse(
        attributeArgumentCaptor.getValue().getAllowedAttributeValues().get(0).isMarkForDelete());
    verify(attributeService).update(attributeArgumentCaptor.getValue());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValuesDefiningAttributeAddSizeAttributeTest1() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setSizeAttribute(true);
    allowedAttributeValue3.setMarkForDelete(true);
    allowedAttributeValue3.setValueType(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    attribute.setAllowedAttributeValues(Arrays.asList(allowedAttributeValue3));
    AllowedAttributeValue allowedAttributeValue3 = new AllowedAttributeValue();
    allowedAttributeValue3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    allowedAttributeValue3.setMarkForDelete(true);
    attributeValueUpdateDTO3.setValueType(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    masterAttributeUpdateDTO.getAddedAttributeValues().add(attributeValueUpdateDTO3);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(attributeArgumentCaptor.capture());
    when(domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.capture())).thenReturn(
        new AttributeDomainEventModel());
    masterAttributeUpdateDTO.setSortType(AttributeSortType.ALPHABETICAL);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE,
        masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(domainEventPublisherServiceBean).publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.getValue());
    assertTrue(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    assertFalse(
        attributeArgumentCaptor.getValue().getAllowedAttributeValues().get(0).isMarkForDelete());
    verify(attributeService).update(attributeArgumentCaptor.getValue());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValuesDefiningAttributeAddSizeAttributeTest2() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setSizeAttribute(true);
    allowedAttributeValue3.setMarkForDelete(true);
    allowedAttributeValue3.setValueType(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    List<AllowedAttributeValue> list = new ArrayList<>();
    list.add(allowedAttributeValue3);
    attribute.setAllowedAttributeValues(list);
    AllowedAttributeValue allowedAttributeValue3 = new AllowedAttributeValue();
    allowedAttributeValue3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    allowedAttributeValue3.setMarkForDelete(true);
    masterAttributeUpdateDTO.getAddedAttributeValues().add(attributeValueUpdateDTO3);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(attributeArgumentCaptor.capture());
    when(domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.capture())).thenReturn(
        new AttributeDomainEventModel());
    masterAttributeUpdateDTO.setSortType(AttributeSortType.ALPHABETICAL);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE,
        masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(domainEventPublisherServiceBean).publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.getValue());
    assertTrue(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    verify(attributeService).update(attributeArgumentCaptor.getValue());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
    verify(allowedAttributeValueService).getSequence(attribute.getAttributeCode());
    verify(allowedAttributeValueService).save(allowedAttributeValueArgumentCaptor.capture());
  }

  @Test
  public void updateAttributeValuesDefiningAttributeAddSizeAttributeTest3() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setSizeAttribute(true);
    allowedAttributeValue3.setMarkForDelete(true);
    List<AllowedAttributeValue> list = new ArrayList<>();
    list.add(allowedAttributeValue3);
    attribute.setAllowedAttributeValues(list);
    AllowedAttributeValue allowedAttributeValue3 = new AllowedAttributeValue();
    allowedAttributeValue3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    allowedAttributeValue3.setMarkForDelete(true);
        attributeValueUpdateDTO3.setValueType(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    masterAttributeUpdateDTO.getAddedAttributeValues().add(attributeValueUpdateDTO3);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(attributeArgumentCaptor.capture());
    when(domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.capture())).thenReturn(
        new AttributeDomainEventModel());
    masterAttributeUpdateDTO.setSortType(AttributeSortType.ALPHABETICAL);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE,
        masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(domainEventPublisherServiceBean).publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.getValue());
    assertTrue(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    verify(attributeService).update(attributeArgumentCaptor.getValue());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
    verify(allowedAttributeValueService).getSequence(attribute.getAttributeCode());
    verify(allowedAttributeValueService).save(allowedAttributeValueArgumentCaptor.capture());
  }

  @Test
  public void updateAttributeValuesDefiningAttributeAddValueTypeAttributeTest() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setSortType(AttributeSortType.MANUAL);
    attribute.setValueTypeAttribute(true);
    allowedAttributeValue3.setMarkForDelete(true);
    attribute.setAllowedAttributeValues(Arrays.asList(allowedAttributeValue3));
    AllowedAttributeValue allowedAttributeValue3 = new AllowedAttributeValue();
    allowedAttributeValue3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    allowedAttributeValue3.setMarkForDelete(true);
    masterAttributeUpdateDTO.getAddedAttributeValues().add(attributeValueUpdateDTO3);
    masterAttributeUpdateDTO.setSortType(AttributeSortType.CUSTOM);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(attributeArgumentCaptor.capture());
    when(domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.capture())).thenReturn(
        new AttributeDomainEventModel());
    masterAttributeUpdateDTO.setSortType(AttributeSortType.ALPHABETICAL);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE,
        masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(domainEventPublisherServiceBean).publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.getValue());
    assertTrue(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    assertFalse(
        attributeArgumentCaptor.getValue().getAllowedAttributeValues().get(0).isMarkForDelete());
    verify(attributeService).update(attributeArgumentCaptor.getValue());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValuesAndValueTypeDefiningAttributeAlphabeticalSortTest()
      throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setSizeAttribute(true);
    allowedAttributeValue3.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_3);
    attribute.setAllowedAttributeValues(Arrays.asList(allowedAttributeValue3));
    AllowedAttributeValue allowedAttributeValue3 = new AllowedAttributeValue();
    allowedAttributeValue3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    allowedAttributeValue3.setMarkForDelete(false);
    attributeValueUpdateDTO3.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_3);
    masterAttributeUpdateDTO.getAttributeValues().add(attributeValueUpdateDTO3);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(attributeArgumentCaptor.capture());
    when(domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.capture())).thenReturn(
        new AttributeDomainEventModel());
    masterAttributeUpdateDTO.setSortType(AttributeSortType.CUSTOM);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE,
        masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(domainEventPublisherServiceBean).publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.getValue());
    assertTrue(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    verify(attributeService).update(attributeArgumentCaptor.getValue());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValuesAndValueTypeDefiningAttributeTest()
      throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setSizeAttribute(true);
    allowedAttributeValue2.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_3);
    List<AllowedAttributeValue> allowedAttributeValues = new ArrayList<>();
    allowedAttributeValues.add(allowedAttributeValue2);
    allowedAttributeValue3.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_1);
    allowedAttributeValue3.setValueType(ATTRIBUTE_VALUE_UPDATE_DTO_CODE_1);
    allowedAttributeValues.add(allowedAttributeValue3);
    attribute.setAllowedAttributeValues(allowedAttributeValues);
    AllowedAttributeValue allowedAttributeValue3 = new AllowedAttributeValue();
    allowedAttributeValue3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    allowedAttributeValue3.setValueType(ATTRIBUTE_VALUE_UPDATE_DTO_CODE_1);
    allowedAttributeValue3.setMarkForDelete(false);
    attributeValueUpdateDTO3.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_3);
    attributeValueUpdateDTO3.setValueType(ATTRIBUTE_VALUE_UPDATE_DTO_CODE_1);
    masterAttributeUpdateDTO.getAttributeValues().add(attributeValueUpdateDTO3);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(attributeArgumentCaptor.capture());
    when(domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.capture())).thenReturn(
        new AttributeDomainEventModel());
    masterAttributeUpdateDTO.setSortType(AttributeSortType.CUSTOM);
    try {
      masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE,
          masterAttributeUpdateDTO);
    } catch (ValidationException e) {
      assertEquals(
          ErrorMessage.VALUE_WITH_VALUE_TYPE_ALREADY_EXISTS.getMessage(),
          e.getErrorMessage());
    }
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValuesAndExistingValueTypeTest() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setValueTypeAttribute(true);
    allowedAttributeValue3.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_3);
    allowedAttributeValue3.setValueType(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    attribute.setAllowedAttributeValues(Arrays.asList(allowedAttributeValue3));
    AllowedAttributeValue allowedAttributeValue3 = new AllowedAttributeValue();
    allowedAttributeValue3.setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    allowedAttributeValue3.setMarkForDelete(false);
    attributeValueUpdateDTO3.setId(ATTRIBUTE_VALUE_UPDATE_DTO_ID_3);
    masterAttributeUpdateDTO.getAttributeValues().add(attributeValueUpdateDTO3);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(attributeArgumentCaptor.capture());
    when(domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.capture())).thenReturn(
        new AttributeDomainEventModel());
    masterAttributeUpdateDTO.setSortType(AttributeSortType.ALPHABETICAL);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE,
        masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(domainEventPublisherServiceBean).publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.getValue());
    assertTrue(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    verify(attributeService).update(attributeArgumentCaptor.getValue());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValues_DefiningAttribute_DeleteNotExistingAttributeValue_Test() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    masterAttributeUpdateDTO.getDeletedAttributeValues().add(attributeValueUpdateDTO5);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(any(Attribute.class));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO));
    } finally {
      verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    }
  }

  @Test
  public void updateAttributeValues_PredefinedAttribute_Test() throws Exception {
    predefinedAllowedAttributeValue3.setMarkForDelete(true);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    masterAttributeUpdateDTO.getAttributeValues().addAll(
        Arrays.asList(attributeValueUpdateDTO1, attributeValueUpdateDTO2));
    masterAttributeUpdateDTO.getAddedAttributeValues().addAll(
        Arrays.asList(attributeValueUpdateDTO3, attributeValueUpdateDTO4));
    masterAttributeUpdateDTO.getDeletedAttributeValues().add(attributeValueUpdateDTO5);
    attribute.setPredefinedAllowedAttributeValues(new ArrayList<>(Arrays.asList(predefinedAllowedAttributeValue1, predefinedAllowedAttributeValue2, predefinedAllowedAttributeValue5)));
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(any(Attribute.class));
    when(predefinedAllowedAttributeValueService.save(any(PredefinedAllowedAttributeValue.class))).thenReturn(ID);
    doNothing().when(predefinedAllowedAttributeValueService).update(any(PredefinedAllowedAttributeValue.class));
    when(predefinedAllowedAttributeValueService.getSequence(ATTRIBUTE_CODE)).thenReturn(ATTRIBUTE_CODE);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService, times(2)).getSequence(ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService, times(2))
        .save(predefinedAllowedAttributeValueArgumentCaptor.capture());
    verify(attributeService).update(attributeArgumentCaptor.capture());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
    assertEquals(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_3,
            predefinedAllowedAttributeValueArgumentCaptor.getAllValues().get(0).getSequence());
    assertEquals(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_4,
            predefinedAllowedAttributeValueArgumentCaptor.getAllValues().get(1).getSequence());
    assertEquals(AttributeSortType.MANUAL, attributeArgumentCaptor.getValue().getSortType());
  }

  @Test
  public void updateAttributeValues_PredefinedMultiVal_Test() throws Exception {
    predefinedAllowedAttributeValue3.setMarkForDelete(true);
    attribute.setAttributeType(AttributeType.PREDEFINED_MULTIVALUE);
    masterAttributeUpdateDTO.getAttributeValues().addAll(
        Arrays.asList(attributeValueUpdateDTO1, attributeValueUpdateDTO2));
    masterAttributeUpdateDTO.getAddedAttributeValues().addAll(
        Arrays.asList(attributeValueUpdateDTO3, attributeValueUpdateDTO4));
    masterAttributeUpdateDTO.getDeletedAttributeValues().add(attributeValueUpdateDTO5);
    attribute.setPredefinedAllowedAttributeValues(new ArrayList<>(Arrays.asList(predefinedAllowedAttributeValue1, predefinedAllowedAttributeValue2, predefinedAllowedAttributeValue5)));
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(any(Attribute.class));
    when(predefinedAllowedAttributeValueService.save(any(PredefinedAllowedAttributeValue.class))).thenReturn(ID);
    doNothing().when(predefinedAllowedAttributeValueService).update(any(PredefinedAllowedAttributeValue.class));
    when(predefinedAllowedAttributeValueService.getSequence(ATTRIBUTE_CODE)).thenReturn(ATTRIBUTE_CODE);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService, times(2)).getSequence(ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService, times(2))
        .save(predefinedAllowedAttributeValueArgumentCaptor.capture());
    verify(attributeService).update(attributeArgumentCaptor.capture());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
    assertEquals(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_3,
        predefinedAllowedAttributeValueArgumentCaptor.getAllValues().get(0).getSequence());
    assertEquals(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_4,
        predefinedAllowedAttributeValueArgumentCaptor.getAllValues().get(1).getSequence());
    assertEquals(AttributeSortType.MANUAL, attributeArgumentCaptor.getValue().getSortType());
  }

  @Test
  public void updateAttributeValues_DescriptiveTest()
      throws Exception { //test case to cover branch coverage
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(any(Attribute.class));
    when(predefinedAllowedAttributeValueService.save(
        any(PredefinedAllowedAttributeValue.class))).thenReturn(ID);
    doNothing().when(predefinedAllowedAttributeValueService)
        .update(any(PredefinedAllowedAttributeValue.class));
    when(predefinedAllowedAttributeValueService.getSequence(ATTRIBUTE_CODE)).thenReturn(
        ATTRIBUTE_CODE);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE,
        masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(attributeService).update(attributeArgumentCaptor.capture());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValues_PredefinedAttribute_Test2() throws Exception {
    predefinedAllowedAttributeValue3.setMarkForDelete(true);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    masterAttributeUpdateDTO.getAttributeValues().addAll(
        Arrays.asList(attributeValueUpdateDTO1, attributeValueUpdateDTO2));
    masterAttributeUpdateDTO.getAddedAttributeValues().addAll(
        Arrays.asList(attributeValueUpdateDTO3, attributeValueUpdateDTO4));
    masterAttributeUpdateDTO.getDeletedAttributeValues().add(attributeValueUpdateDTO5);
    attribute.setPredefinedAllowedAttributeValues(new ArrayList<>(Arrays.asList(predefinedAllowedAttributeValue1, predefinedAllowedAttributeValue2, predefinedAllowedAttributeValue5)));
    attribute.getPredefinedAllowedAttributeValues().get(2).setMarkForDelete(true);
    attribute.getPredefinedAllowedAttributeValues().get(2).setValue(ATTRIBUTE_VALUE_UPDATE_VALUE_3);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(attributeService.save(any(Attribute.class))).thenReturn(ID);
    doNothing().when(attributeService).update(any(Attribute.class));
    when(predefinedAllowedAttributeValueService.save(any(PredefinedAllowedAttributeValue.class))).thenReturn(ID);
    doNothing().when(predefinedAllowedAttributeValueService).update(any(PredefinedAllowedAttributeValue.class));
    when(predefinedAllowedAttributeValueService.getSequence(ATTRIBUTE_CODE)).thenReturn(ATTRIBUTE_CODE);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService).getSequence(ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService)
        .save(predefinedAllowedAttributeValueArgumentCaptor.capture());
    verify(attributeService).update(attributeArgumentCaptor.capture());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
    assertEquals(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_4,
        predefinedAllowedAttributeValueArgumentCaptor.getAllValues().get(0).getSequence());
    assertEquals(AttributeSortType.MANUAL, attributeArgumentCaptor.getValue().getSortType());
  }

  @Test
  public void updateAttributeValues_AddPredefinedAttributeWithExistingValues_Test() throws Exception {
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    predefinedAllowedAttributeValue3.setMarkForDelete(false);
    predefinedAllowedAttributeValue4.setMarkForDelete(false);
    predefinedAllowedAttributeValueDuplicate3.setMarkForDelete(false);
    predefinedAllowedAttributeValueDuplicate4.setMarkForDelete(true);
    masterAttributeUpdateDTO.getAddedAttributeValues().addAll(
        Arrays.asList(attributeValueUpdateDTO3, attributeValueUpdateDTO4));
    attribute.setPredefinedAllowedAttributeValues(
        new ArrayList<>(Arrays.asList(predefinedAllowedAttributeValue3, predefinedAllowedAttributeValue4)));
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    try {
      masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO);
    }catch (ApplicationRuntimeException e){

    }finally {
      verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    }
  }

  @Test
  public void updateAttributeValues_AddPredefinedAttributeWithExistingValuesTest() throws Exception {
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    predefinedAllowedAttributeValue4.setMarkForDelete(false);
    predefinedAllowedAttributeValueDuplicate4.setMarkForDelete(true);
    masterAttributeUpdateDTO.getAddedAttributeValues().addAll(Arrays.asList(attributeValueUpdateDTO4));
    attribute.setPredefinedAllowedAttributeValues(new ArrayList<>(
        Arrays.asList(predefinedAllowedAttributeValueDuplicate4, predefinedAllowedAttributeValueDuplicate3)));
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(attributeService).update(attributeArgumentCaptor.capture());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValuesAddPredefinedAttributeWithExistingValuesCustomTest() throws Exception {
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    predefinedAllowedAttributeValue4.setMarkForDelete(false);
    predefinedAllowedAttributeValueDuplicate4.setMarkForDelete(true);
    masterAttributeUpdateDTO.getAddedAttributeValues().addAll(Arrays.asList(attributeValueUpdateDTO4));
    attribute.setPredefinedAllowedAttributeValues(new ArrayList<>(
        Arrays.asList(predefinedAllowedAttributeValueDuplicate4, predefinedAllowedAttributeValueDuplicate3)));
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    masterAttributeUpdateDTO.setSortType(AttributeSortType.CUSTOM);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(attributeService).update(attributeArgumentCaptor.capture());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValuesAddPredefinedAttributeWithExistingValuesAlphabeticalTest() throws Exception {
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    predefinedAllowedAttributeValue4.setMarkForDelete(false);
    predefinedAllowedAttributeValueDuplicate4.setMarkForDelete(true);
    masterAttributeUpdateDTO.getAddedAttributeValues().addAll(Arrays.asList(attributeValueUpdateDTO4));
    attribute.setPredefinedAllowedAttributeValues(new ArrayList<>(
        Arrays.asList(predefinedAllowedAttributeValueDuplicate4, predefinedAllowedAttributeValueDuplicate3)));
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    masterAttributeUpdateDTO.setSortType(AttributeSortType.ALPHABETICAL);
    masterAttributeServiceBean.updateAttributeValues(STORE_ID, ATTRIBUTE_CODE, masterAttributeUpdateDTO);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(attributeService).update(attributeArgumentCaptor.capture());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
  }

  @Test
  public void testUpdateMasterAttribute() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setUpdatedBy(UPDATED_BY);
    attribute.setVariantCreation(VARIANT_CREATION);
    AttributeDomainEventModel attributeDomainEventModel =
        ConverterUtil.getMasterAttributeDomainEventModel(attribute);
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setDsExtraction(false);
    attribute.setHideForCustomer(true);
    attribute.setHideForSeller(true);
    attribute.setId(uuid);
    attribute.setDsExtraction(false);
    when(this.domainEventPublisherServiceBean
        .publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.capture()))
        .thenReturn(attributeDomainEventModel);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId()))
        .thenReturn(attribute);
    when(this.categoryAttributeRepository
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId()))
        .thenReturn(Collections.EMPTY_LIST);
    when(this.repository.saveAndFlush(attribute)).thenReturn(attribute);
    Attribute result = masterAttributeServiceBean.updateMasterAttribute(attribute, new ArrayList<>());
    assertEquals(attribute, result);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
    verify(this.repository, AT_LEAST_ONCE).saveAndFlush(attribute);
    verify(this.attributeService)
        .evictAttributeCache(attribute.getStoreId(), attribute.getId(), attribute.getAttributeCode());
    verify(this.categoryAttributeRepository)
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
    verify(this.domainEventPublisherServiceBean)
        .publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.getValue());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    assertNull(attribute.getDescriptionSearch());
    assertFalse(result.isDsExtraction());
    assertTrue(result.isHideForCustomer());
    assertTrue(result.isHideForSeller());
  }

  @Test
  public void testUpdateMasterAttributeForDsExtraction() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setUpdatedBy(UPDATED_BY);
    attribute.setVariantCreation(VARIANT_CREATION);
    AttributeDomainEventModel attributeDomainEventModel =
        ConverterUtil.getMasterAttributeDomainEventModel(attribute);
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setDsExtraction(true);
    attribute.setHideForCustomer(true);
    attribute.setHideForSeller(true);
    attribute.setId(uuid);

    Attribute savedAttribute = this.getDefaultAttribute();
    savedAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    savedAttribute.setUpdatedBy(UPDATED_BY);
    savedAttribute.setVariantCreation(VARIANT_CREATION);
    savedAttribute.setDsExtraction(false);
    savedAttribute.setHideForCustomer(true);
    savedAttribute.setHideForSeller(true);
    savedAttribute.setId(uuid);

    when(this.domainEventPublisherServiceBean
        .publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.capture()))
        .thenReturn(attributeDomainEventModel);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId()))
        .thenReturn(savedAttribute);
    when(this.categoryAttributeRepository
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId()))
        .thenReturn(Collections.EMPTY_LIST);
    when(this.repository.saveAndFlush(attribute)).thenReturn(attribute);
    Attribute result = masterAttributeServiceBean.updateMasterAttribute(attribute, new ArrayList<>());
    assertEquals(attribute, result);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
    verify(this.repository, AT_LEAST_ONCE).saveAndFlush(attribute);
    verify(this.attributeService)
        .evictAttributeCache(attribute.getStoreId(), attribute.getId(), attribute.getAttributeCode());
    verify(this.categoryAttributeRepository)
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
    verify(this.domainEventPublisherServiceBean)
        .publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.getValue());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    assertNull(attribute.getDescriptionSearch());
    assertTrue(result.isDsExtraction());
    assertTrue(result.isHideForCustomer());
    assertTrue(result.isHideForSeller());
    assertEquals(List.of(DS_EXTRACTION), attributeDomainEventModelArgumentCaptor.getValue().getUpdatedFields());
  }

  @Test
  public void testUpdateMasterAttributeWithOutDsExtrationUpdate() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setUpdatedBy(UPDATED_BY);
    attribute.setVariantCreation(VARIANT_CREATION);
    AttributeDomainEventModel attributeDomainEventModel =
        ConverterUtil.getMasterAttributeDomainEventModel(attribute);
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setDsExtraction(true);
    attribute.setHideForCustomer(true);
    attribute.setHideForSeller(true);
    attribute.setId(uuid);

    Attribute savedAttribute = this.getDefaultAttribute();
    savedAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    savedAttribute.setUpdatedBy(UPDATED_BY);
    savedAttribute.setVariantCreation(VARIANT_CREATION);
    savedAttribute.setDsExtraction(true);
    savedAttribute.setHideForCustomer(true);
    savedAttribute.setHideForSeller(true);
    savedAttribute.setId(uuid);

    when(this.domainEventPublisherServiceBean
        .publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.capture()))
        .thenReturn(attributeDomainEventModel);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId()))
        .thenReturn(savedAttribute);
    when(this.categoryAttributeRepository
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId()))
        .thenReturn(Collections.EMPTY_LIST);
    when(this.repository.saveAndFlush(attribute)).thenReturn(attribute);
    Attribute result = masterAttributeServiceBean.updateMasterAttribute(attribute, new ArrayList<>());
    assertEquals(attribute, result);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
    verify(this.repository, AT_LEAST_ONCE).saveAndFlush(attribute);
    verify(this.attributeService)
        .evictAttributeCache(attribute.getStoreId(), attribute.getId(), attribute.getAttributeCode());
    verify(this.categoryAttributeRepository)
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
    verify(this.domainEventPublisherServiceBean)
        .publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.getValue());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    assertNull(attribute.getDescriptionSearch());
    assertTrue(result.isDsExtraction());
    assertTrue(result.isHideForCustomer());
    assertTrue(result.isHideForSeller());
    assertEquals(new ArrayList<>(), attributeDomainEventModelArgumentCaptor.getValue().getUpdatedFields());
  }

  @Test
  public void testUpdateMasterAttributeWithDsExtrationUpdateWithFalse() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setUpdatedBy(UPDATED_BY);
    attribute.setVariantCreation(VARIANT_CREATION);
    AttributeDomainEventModel attributeDomainEventModel =
        ConverterUtil.getMasterAttributeDomainEventModel(attribute);
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setDsExtraction(false);
    attribute.setHideForCustomer(true);
    attribute.setHideForSeller(true);
    attribute.setId(uuid);

    Attribute savedAttribute = this.getDefaultAttribute();
    savedAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    savedAttribute.setUpdatedBy(UPDATED_BY);
    savedAttribute.setVariantCreation(VARIANT_CREATION);
    savedAttribute.setDsExtraction(true);
    savedAttribute.setHideForCustomer(true);
    savedAttribute.setHideForSeller(true);
    savedAttribute.setId(uuid);

    when(this.domainEventPublisherServiceBean
        .publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.capture()))
        .thenReturn(attributeDomainEventModel);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId()))
        .thenReturn(savedAttribute);
    when(this.categoryAttributeRepository
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId()))
        .thenReturn(Collections.EMPTY_LIST);
    when(this.repository.saveAndFlush(attribute)).thenReturn(attribute);
    Attribute result = masterAttributeServiceBean.updateMasterAttribute(attribute, new ArrayList<>());
    assertEquals(attribute, result);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
    verify(this.repository, AT_LEAST_ONCE).saveAndFlush(attribute);
    verify(this.attributeService)
        .evictAttributeCache(attribute.getStoreId(), attribute.getId(), attribute.getAttributeCode());
    verify(this.categoryAttributeRepository)
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
    verify(this.domainEventPublisherServiceBean)
        .publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.getValue());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    assertNull(attribute.getDescriptionSearch());
    assertFalse(result.isDsExtraction());
    assertTrue(result.isHideForCustomer());
    assertTrue(result.isHideForSeller());
    assertEquals(List.of(DS_EXTRACTION), attributeDomainEventModelArgumentCaptor.getValue().getUpdatedFields());
  }

  @Test
  public void testUpdateMasterSizeAttribute() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setUpdatedBy(UPDATED_BY);
    attribute.setVariantCreation(VARIANT_CREATION);
    attribute.setSizeAttribute(true);
    List<String> valueTypes = new ArrayList<>(Collections.singleton(VALUE));
    attribute.setValueTypes(VALUE_TYPES_JSON);
    AttributeDomainEventModel attributeDomainEventModel =
        ConverterUtil.getMasterAttributeDomainEventModel(attribute);
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setId(uuid);
    attribute.setDsExtraction(true);
    attribute.setHideForCustomer(true);
    attribute.setHideForSeller(true);
    TypeReference<List<String>> typeRef = new TypeReference<>() {};
    SystemParameter systemParameter =
        new SystemParameter(Constants.VALUE_TYPES_CONFIG, VALUE, DESCRIPTION);
    when(this.mapper.readValue(Mockito.anyString(), any(TypeReference.class))).thenReturn(
        objectMapper.readValue(attribute.getValueTypes(), typeRef));
    when(this.domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.capture())).thenReturn(attributeDomainEventModel);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(),
        attribute.getId())).thenReturn(attribute);
    when(this.systemParameterService.findByStoreIdAndVariable(STORE_ID,
        Constants.VALUE_TYPES_CONFIG)).thenReturn(systemParameter);
    when(
        this.categoryAttributeRepository.findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(
            attribute.getStoreId(), attribute.getId())).thenReturn(Collections.EMPTY_LIST);
    when(this.repository.saveAndFlush(attribute)).thenReturn(attribute);
    Attribute result = masterAttributeServiceBean.updateMasterAttribute(attribute, valueTypes);
    assertEquals(attribute, result);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(),
        attribute.getId());
    verify(this.repository, AT_LEAST_ONCE).saveAndFlush(attribute);
    verify(this.attributeService).evictAttributeCache(attribute.getStoreId(), attribute.getId(),
        attribute.getAttributeCode());
    verify(
        this.categoryAttributeRepository).findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(
        attribute.getStoreId(), attribute.getId());
    verify(this.domainEventPublisherServiceBean).publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.getValue());
    verify(this.systemParameterService).findByStoreIdAndVariable(STORE_ID,
        Constants.VALUE_TYPES_CONFIG);
    verify(this.mapper).readValue(Mockito.anyString(), any(TypeReference.class));
    verify(this.mapper).writeValueAsString(valueTypes);
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    assertNull(attribute.getDescriptionSearch());
    assertTrue(result.isDsExtraction());
    assertTrue(result.isHideForCustomer());
    assertTrue(result.isHideForSeller());
  }

  @Test
  public void testUpdateMasterSizeAttributeWithoutValueTypes() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setUpdatedBy(UPDATED_BY);
    attribute.setVariantCreation(VARIANT_CREATION);
    attribute.setSizeAttribute(true);
    List<String> valueTypes = new ArrayList<>(Collections.singleton(VALUE));
    AttributeDomainEventModel attributeDomainEventModel =
        ConverterUtil.getMasterAttributeDomainEventModel(attribute);
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setId(uuid);
    TypeReference<List<String>> typeRef = new TypeReference<>() {};
    SystemParameter systemParameter =
        new SystemParameter(Constants.VALUE_TYPES_CONFIG, VALUE, DESCRIPTION);
    when(this.domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.capture())).thenReturn(attributeDomainEventModel);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(),
        attribute.getId())).thenReturn(attribute);
    when(this.systemParameterService.findByStoreIdAndVariable(STORE_ID,
        Constants.VALUE_TYPES_CONFIG)).thenReturn(systemParameter);
    when(
        this.categoryAttributeRepository.findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(
            attribute.getStoreId(), attribute.getId())).thenReturn(Collections.EMPTY_LIST);
    when(this.repository.saveAndFlush(attribute)).thenReturn(attribute);
    Attribute result = masterAttributeServiceBean.updateMasterAttribute(attribute, valueTypes);
    assertEquals(attribute, result);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(),
        attribute.getId());
    verify(this.repository, AT_LEAST_ONCE).saveAndFlush(attribute);
    verify(this.attributeService).evictAttributeCache(attribute.getStoreId(), attribute.getId(),
        attribute.getAttributeCode());
    verify(
        this.categoryAttributeRepository).findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(
        attribute.getStoreId(), attribute.getId());
    verify(this.domainEventPublisherServiceBean).publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.getValue());
    verify(this.systemParameterService).findByStoreIdAndVariable(STORE_ID,
        Constants.VALUE_TYPES_CONFIG);
    verify(this.mapper).writeValueAsString(valueTypes);
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    assertNull(attribute.getDescriptionSearch());
  }

  @Test
  public void testUpdateMasterSizeAttributeExisting() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setUpdatedBy(UPDATED_BY);
    attribute.setVariantCreation(VARIANT_CREATION);
    Attribute existingAttribute = new Attribute();
    attribute.setSizeAttribute(true);
    BeanUtils.copyProperties(attribute,existingAttribute);
    existingAttribute.setSizeAttribute(false);
    List<String> valueTypes = new ArrayList<>(Collections.singleton(VALUE));
    AttributeDomainEventModel attributeDomainEventModel =
        ConverterUtil.getMasterAttributeDomainEventModel(attribute);
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setId(uuid);
    TypeReference<List<String>> typeRef = new TypeReference<>() {};
    SystemParameter systemParameter =
        new SystemParameter(Constants.VALUE_TYPES_CONFIG, VALUE, DESCRIPTION);
    when(this.domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.capture())).thenReturn(attributeDomainEventModel);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(),
        attribute.getId())).thenReturn(existingAttribute);
    when(this.systemParameterService.findByStoreIdAndVariable(STORE_ID,
        Constants.VALUE_TYPES_CONFIG)).thenReturn(systemParameter);
    when(
        this.categoryAttributeRepository.findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(
            attribute.getStoreId(), attribute.getId())).thenReturn(Collections.EMPTY_LIST);
    when(this.repository.saveAndFlush(attribute)).thenReturn(attribute);
    Attribute result = masterAttributeServiceBean.updateMasterAttribute(attribute, valueTypes);
    assertEquals(attribute, result);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(),
        attribute.getId());
    verify(this.repository, AT_LEAST_ONCE).saveAndFlush(attribute);
    verify(this.attributeService).evictAttributeCache(attribute.getStoreId(), attribute.getId(),
        attribute.getAttributeCode());
    verify(
        this.categoryAttributeRepository).findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(
        attribute.getStoreId(), attribute.getId());
    verify(this.domainEventPublisherServiceBean).publishMasterAttributeInfoEvent(
        attributeDomainEventModelArgumentCaptor.getValue());
    verify(this.systemParameterService).findByStoreIdAndVariable(STORE_ID,
        Constants.VALUE_TYPES_CONFIG);
    verify(this.mapper).writeValueAsString(valueTypes);
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isValueUpdate());
    assertFalse(attributeDomainEventModelArgumentCaptor.getValue().isNewAttribute());
    assertNull(attribute.getDescriptionSearch());
  }

  @Test
  public void testUpdateMasterSizeAttributeValidation() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setUpdatedBy(UPDATED_BY);
    attribute.setVariantCreation(VARIANT_CREATION);
    Attribute existingAttribute = new Attribute();
    BeanUtils.copyProperties(attribute, existingAttribute);
    existingAttribute.setSizeAttribute(true);
    List<String> valueTypes = new ArrayList<>(Collections.singleton(VALUE));
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setId(uuid);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(),
        attribute.getId())).thenReturn(existingAttribute);
    try {
      Attribute result = masterAttributeServiceBean.updateMasterAttribute(attribute, valueTypes);
    } catch (ValidationException ex) {
      assertEquals(ex.getErrorMessage(),
          ErrorMessage.SIZE_ATTRIBUTE_TYPE_CANNOT_BE_CHANGED.toString());
    }
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(),
        attribute.getId());
  }

  @Test
  public void testUpdateMasterAttributeNotNullDescription() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setUpdatedBy(UPDATED_BY);
    attribute.setDescription(DESCRIPTION.getBytes());
    attribute.setVariantCreation(VARIANT_CREATION);
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setId(uuid);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId()))
        .thenReturn(attribute);
    when(this.categoryAttributeRepository
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId()))
        .thenReturn(Collections.EMPTY_LIST);
    when(this.repository.saveAndFlush(attribute)).thenReturn(attribute);
    Attribute result = masterAttributeServiceBean.updateMasterAttribute(attribute, new ArrayList<>());
    assertEquals(attribute, result);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
    verify(this.repository, AT_LEAST_ONCE).saveAndFlush(attribute);
    verify(this.categoryAttributeRepository)
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
    verify(this.attributeService)
        .evictAttributeCache(attribute.getStoreId(), attribute.getId(), attribute.getAttributeCode());
    assertEquals(DESCRIPTION, result.getDescriptionSearch());
  }

  @Test
  public void testUpdateMasterAttributeWithException() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setUpdatedBy(UPDATED_BY);
    attribute.setVariantCreation(VARIANT_CREATION);
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setId(uuid);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId()))
        .thenReturn(null);
    Attribute result = null;
    try {
      result = masterAttributeServiceBean.updateMasterAttribute(attribute, new ArrayList<>());
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationException);
      ApplicationException applicationException = (ApplicationException) e;
      if (applicationException.getErrorMessage()
          .contains(MasterAttributeServiceBean.INVALID_ATT_ID)) {
        assertTrue(true);
        assertNull(result);
      } else {
        assertTrue(false);
      }
    }
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
  }

  @Test
  public void testUpdateMasterAttributeWithApplicationRuntimeException() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setVariantCreation(VARIANT_CREATION);
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setId(uuid);
    Attribute result = null;
    try {
      result = masterAttributeServiceBean.updateMasterAttribute(attribute, new ArrayList<>());
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      assertNull(result);
    }
  }


  private List<Attribute> getDefaultAttributeList() {
    List<Attribute> attributes = new ArrayList<Attribute>();
    attributes.add(new Attribute(MasterAttributeServiceTest.ATTRIBUTE1_NAME, AttributeType.DEFINING_ATTRIBUTE, false,
        MasterAttributeServiceTest.STORE_ID));
    attributes.add(new Attribute(MasterAttributeServiceTest.ATTRIBUTE2_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE, true,
        MasterAttributeServiceTest.STORE_ID));
    attributes.add(new Attribute(MasterAttributeServiceTest.ATTRIBUTE3_NAME, AttributeType.DEFINING_ATTRIBUTE, true,
        MasterAttributeServiceTest.STORE_ID));
    return attributes;
  }

  private Page<Attribute> getDefaultAttributePage() {
    return new PageImpl<Attribute>(this.getDefaultAttributeList(), MasterAttributeServiceTest.DEFAULT_PAGE_REQUEST,
        this.getDefaultAttributeList().size());
  }

  @Test
  public void getAttributeByAttributeFilter() {
    Mockito.when(
      this.repository.findByStoreIdAndAttributeTypeAndNameAndSizeAttributeAndMarkForDeleteFalseOrderByName(STORE_ID,
        AttributeType.valueOf(attributeFilter.getAttributeType()), attributeFilter.getName(),
        attributeFilter.getSortedBy(), attributeFilter.getSortDirection(), DEFAULT_PAGE_REQUEST,
        attributeFilter.getSizeAttribute())).thenReturn(getDefaultAttributePage());
    this.masterAttributeServiceBean.getAttributeByAttributeFilter(STORE_ID, attributeFilter,
      DEFAULT_PAGE_REQUEST);
    Mockito.verify(this.repository)
      .findByStoreIdAndAttributeTypeAndNameAndSizeAttributeAndMarkForDeleteFalseOrderByName(STORE_ID,
        AttributeType.valueOf(attributeFilter.getAttributeType()), attributeFilter.getName(),
        attributeFilter.getSortedBy(), attributeFilter.getSortDirection(), DEFAULT_PAGE_REQUEST,
        attributeFilter.getSizeAttribute());
  }

  @Test
  public void getAttributeByAttributeFilterSizeAttributeFilterTest() {
    attributeFilter.setSizeAttribute(true);
    Mockito.when(
      this.repository.findByStoreIdAndAttributeTypeAndNameAndSizeAttributeAndMarkForDeleteFalseOrderByName(STORE_ID,
        AttributeType.valueOf(attributeFilter.getAttributeType()), attributeFilter.getName(),
        attributeFilter.getSortedBy(), attributeFilter.getSortDirection(), DEFAULT_PAGE_REQUEST,
        attributeFilter.getSizeAttribute())).thenReturn(getDefaultAttributePage());
    this.masterAttributeServiceBean.getAttributeByAttributeFilter(STORE_ID, attributeFilter,
      DEFAULT_PAGE_REQUEST);
    Mockito.verify(this.repository)
      .findByStoreIdAndAttributeTypeAndNameAndSizeAttributeAndMarkForDeleteFalseOrderByName(STORE_ID,
        AttributeType.valueOf(attributeFilter.getAttributeType()), attributeFilter.getName(),
        attributeFilter.getSortedBy(), attributeFilter.getSortDirection(), DEFAULT_PAGE_REQUEST,
        attributeFilter.getSizeAttribute());
  }

  @Test
  public void addAttributeValue_withExistingDefiningAllowedAttributeValue_Test() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(eq(STORE_ID), eq(attribute),
            eq(attributeValueUpdateDTO1.getValue()))).thenReturn(allowedAttributeValue);
    try {
      masterAttributeServiceBean
          .addAttributeValue(STORE_ID, ATTRIBUTE_CODE, attributeValueUpdateDTO1, USER_NAME, new Date());
    } catch (Exception e) {
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e;
      assertTrue(applicationException.getErrorMessage()
          .contains(EXISTING_ALLOWED_ATTRIBUTE_VALUE + ATTRIBUTE_VALUE_UPDATE_VALUE_1));
    } finally {
      verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
      verify(allowedAttributeValueService).findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(STORE_ID, attribute,
          attributeValueUpdateDTO1.getValue());
    }
  }

  @Test
  public void addAttributeValue_withDefiningAllowedAttributeValue_Test() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(eq(STORE_ID), eq(attribute),
            eq(attributeValueUpdateDTO1.getValue()))).thenReturn(null);
    when(allowedAttributeValueService.getSequence(attribute.getAttributeCode())).thenReturn(SEQUENCE.toString());
    when(allowedAttributeValueService.save(allowedAttributeValue)).thenReturn(ID);
    doNothing().when(attributeService).update(any(Attribute.class));

    AttributeValueResponse attributeValueResponse = masterAttributeServiceBean
        .addAttributeValue(STORE_ID, ATTRIBUTE_CODE, attributeValueUpdateDTO1, USER_NAME, new Date());

    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(allowedAttributeValueService).findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(STORE_ID, attribute,
        attributeValueUpdateDTO1.getValue());
    verify(allowedAttributeValueService).getSequence(attribute.getAttributeCode());
    verify(allowedAttributeValueService).save(any(AllowedAttributeValue.class));
    verify(attributeService).update(attributeArgumentCaptor.capture());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
    verify(allowedAttributeValueService).findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(STORE_ID, attribute,
        attributeValueUpdateDTO1.getValue());
    assertEquals(allowedAttributeValue.getValue(), attributeValueResponse.getValue());
    assertEquals(ALLOWED_ATTRIBUTE_CODE, attributeValueResponse.getAllowedAttributeCode());
  }


  @Test
  public void addAttributeValueMFDFalseValueTest() throws Exception {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(eq(STORE_ID), eq(attribute),
            eq(attributeValueUpdateDTO1.getValue()))).thenReturn(null);
    when(allowedAttributeValueService.findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(STORE_ID, attribute,
        attributeValueUpdateDTO1.getValue())).thenReturn(allowedAttributeValue);
    doNothing().when(attributeService).update(any(Attribute.class));

    AttributeValueResponse attributeValueResponse = masterAttributeServiceBean
        .addAttributeValue(STORE_ID, ATTRIBUTE_CODE, attributeValueUpdateDTO1, USER_NAME, new Date());

    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(allowedAttributeValueService).findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(STORE_ID, attribute,
        attributeValueUpdateDTO1.getValue());
    verify(allowedAttributeValueService).update(allowedAttributeValue);
    verify(attributeService).update(attributeArgumentCaptor.capture());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
    verify(allowedAttributeValueService).findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(STORE_ID, attribute,
        attributeValueUpdateDTO1.getValue());
    assertEquals(attributeValueResponse.getValue(), allowedAttributeValue.getValue());
    assertEquals(attributeValueResponse.getAllowedAttributeCode(), ATTRIBUTE_CODE);
  }

  @Test
  public void addAttributeValue_withExistingPredefinedAllowedAttributeValue_Test() throws Exception {
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(eq(STORE_ID), eq(attribute),
            eq(attributeValueUpdateDTO1.getValue()))).thenReturn(predefinedAllowedAttributeValue);
    try {
      masterAttributeServiceBean
          .addAttributeValue(STORE_ID, ATTRIBUTE_CODE, attributeValueUpdateDTO1, USER_NAME, new Date());
    } catch (Exception e) {
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e;
      assertTrue(applicationException.getErrorMessage()
          .contains(EXISTING_ALLOWED_ATTRIBUTE_VALUE + attributeValueUpdateDTO1.getValue()));
    } finally {
      verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
      verify(predefinedAllowedAttributeValueService)
          .findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(STORE_ID, attribute,
              attributeValueUpdateDTO1.getValue());
    }
  }

  @Test
  public void addAttributeValue_withPredefinedAllowedAttributeValue_Test() throws Exception {
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(eq(STORE_ID), eq(attribute),
            eq(attributeValueUpdateDTO1.getValue()))).thenReturn(null);
    when(predefinedAllowedAttributeValueService.getSequence(attribute.getAttributeCode()))
        .thenReturn(SEQUENCE.toString());
    when(predefinedAllowedAttributeValueService.save(predefinedAllowedAttributeValue)).thenReturn(ID);
    doNothing().when(attributeService).update(any(Attribute.class));

    AttributeValueResponse attributeValueResponse = masterAttributeServiceBean
        .addAttributeValue(STORE_ID, ATTRIBUTE_CODE, attributeValueUpdateDTO1, USER_NAME, new Date());

    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(STORE_ID, attribute,
            attributeValueUpdateDTO1.getValue());
    verify(predefinedAllowedAttributeValueService).getSequence(attribute.getAttributeCode());
    verify(predefinedAllowedAttributeValueService).save(any(PredefinedAllowedAttributeValue.class));
    verify(attributeService).update(attributeArgumentCaptor.capture());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService).
        findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(STORE_ID, attribute,
            attributeValueUpdateDTO1.getValue());
    assertEquals(predefinedAllowedAttributeValue.getValue(), attributeValueResponse.getValue());
    assertEquals(ALLOWED_ATTRIBUTE_CODE, attributeValueResponse.getPredefinedAllowedAttributeCode());
  }

  @Test
  public void addAttributePredefinedValueMFDTrueValueTest() throws Exception {
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(eq(STORE_ID), eq(attribute),
            eq(attributeValueUpdateDTO1.getValue()))).thenReturn(null);
    when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(eq(STORE_ID), eq(attribute),
            eq(attributeValueUpdateDTO1.getValue()))).thenReturn(predefinedAllowedAttributeValue);
    doNothing().when(attributeService).update(any(Attribute.class));

    AttributeValueResponse attributeValueResponse = masterAttributeServiceBean
        .addAttributeValue(STORE_ID, ATTRIBUTE_CODE, attributeValueUpdateDTO1, USER_NAME, new Date());

    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(STORE_ID, attribute,
            attributeValueUpdateDTO1.getValue());
    verify(predefinedAllowedAttributeValueService).update(predefinedAllowedAttributeValue);
    verify(attributeService).update(attributeArgumentCaptor.capture());
    verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService).
        findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(STORE_ID, attribute,
            attributeValueUpdateDTO1.getValue());
    assertEquals(attributeValueResponse.getValue(), predefinedAllowedAttributeValue.getValue());
    assertEquals(attributeValueResponse.getPredefinedAllowedAttributeCode(), ATTRIBUTE_CODE);
  }

  @Test
  public void testUpdateMasterAttributeCategoryCacheEvictionTest() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setUpdatedBy(UPDATED_BY);
    attribute.setVariantCreation(VARIANT_CREATION);
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setId(uuid);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId()))
        .thenReturn(attribute);
    when(this.categoryAttributeRepository
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId()))
        .thenReturn(Collections.singletonList(CATEGORY_ID));
    doNothing().when(applicationCacheServiceBean).clearCategoryDetails(STORE_ID, Collections.singletonList(CATEGORY_ID));
    when(this.repository.saveAndFlush(attribute)).thenReturn(attribute);
    Attribute result = masterAttributeServiceBean.updateMasterAttribute(attribute, new ArrayList<>());
    assertEquals(attribute, result);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
    verify(this.repository, AT_LEAST_ONCE).saveAndFlush(attribute);
    verify(this.attributeService)
        .evictAttributeCache(attribute.getStoreId(), attribute.getId(), attribute.getAttributeCode());
    verify(this.categoryAttributeRepository)
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
    Mockito.verify(applicationCacheServiceBean).clearCategoryDetails(STORE_ID, Collections.singletonList(CATEGORY_ID));
    assertNull(attribute.getDescriptionSearch());
  }
}
