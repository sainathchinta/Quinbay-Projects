package com.gdn.x.productcategorybase.service.impl;

import com.gdn.x.productcategorybase.dto.AttributeBasicDetailDTO;
import com.gdn.x.productcategorybase.dto.SizeChartFilterRequestDTO;
import com.gdn.x.productcategorybase.dto.request.SizeChartFilterRequest;
import com.gdn.x.productcategorybase.dto.response.SizeChartDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SizeChartResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.SizeChart;
import com.gdn.x.productcategorybase.exception.ValidationException;
import com.gdn.x.productcategorybase.repository.SizeChartRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.dto.Gender;
import com.gdn.x.productcategorybase.dto.SizeChartHeaderType;
import com.gdn.x.productcategorybase.dto.request.SizeChartDataColumn;
import com.gdn.x.productcategorybase.dto.request.SizeChartDataRow;
import com.gdn.x.productcategorybase.dto.request.SizeChartRequest;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.CategoryService;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertThrows;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Arrays;
import java.util.Collections;

import java.util.ArrayList;
import java.util.List;

public class SizeChartServiceBeanTest {

  private static final String NAME = "sizeChartName";
  private static final String SIZE_CHART_CODE = "sizeChartCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String BRAND = "brand";
  private static final String NEW_BRAND_CODE = "newBrandCode";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String INTERNATIONAL = "International";
  private static final String KIDS = "Kids";
  private static final String DIMENSION_CODE_1 = "DIM-000001";
  private static final String DIMENSION_CODE_2 = "DIM-000001";
  private static final String FOOT_LENGTH = "Foot length";
  private static final String SHOULDER_LENGTH = "Shoulder length";
  private static final String UNIT = "cm";
  private static final String BUSINESS_PARTNER_CODE = "INTERNAL";
  private static final String PREFIX_SIZE_CHART_CODE = "SIZ";
  private static final String SIZE_CHART_NAME = "sizeChartName";

  private static final String STORE_ID = "STORE_ID";
  private static final String DESCRIPTION = "description";

  @InjectMocks
  private SizeChartServiceBean sizeChartServiceBean;

  @Mock
  private SizeChartRepository sizeChartRepository;

  @Mock
  private ObjectMapper mapper;

  @Mock
  private CategoryService categoryService;

  @Mock
  private AttributeService attributeService;

  private SizeChartFilterRequest sizeChartFilterRequest;
  private Page<SizeChart> sizeChartPage;

  @Captor
  ArgumentCaptor<SizeChart> sizeChartArgumentCaptor;

  private SizeChartRequest sizeChartRequest;
  private List<SizeChartDataColumn> sizeChartDataColumns;
  private SizeChartDataRow sizeChartDataRow;
  private SizeChart sizeChart;

  @BeforeEach
  public void setUp() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    MockitoAnnotations.initMocks(this);
    sizeChartFilterRequest = new SizeChartFilterRequest();
    sizeChart = new SizeChart();
    sizeChart.setSizeAttributeCode(ATTRIBUTE_CODE);
    sizeChartPage = new PageImpl<>(Collections.singletonList(sizeChart));
    sizeChartRequest = new SizeChartRequest();
    sizeChartRequest.setName(NAME);
    sizeChartRequest.setBrand(BRAND);
    sizeChartRequest.setGender(Gender.FEMALE.name());
    sizeChartRequest.setBrandCode(NEW_BRAND_CODE);
    sizeChartRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    sizeChartRequest.setSizeAttributeName(ATTRIBUTE_CODE);
    sizeChartRequest.setUnit(UNIT);
    sizeChartRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    sizeChartRequest.setSelectedValueTypes(List.of(INTERNATIONAL, KIDS));
    sizeChartRequest.setSelectedDimensionCodes(
      new ArrayList<>(List.of(DIMENSION_CODE_1, DIMENSION_CODE_2)));
    SizeChartDataColumn c1 = new SizeChartDataColumn();
    c1.setKeyName(INTERNATIONAL);
    c1.setKeyType(SizeChartHeaderType.VALUE_TYPE.name());
    c1.setValue(INTERNATIONAL);
    SizeChartDataColumn c2 = new SizeChartDataColumn();
    c2.setKeyName(KIDS);
    c2.setKeyType(SizeChartHeaderType.VALUE_TYPE.name());
    c2.setValue(KIDS);
    SizeChartDataColumn c3 = new SizeChartDataColumn();
    c3.setKeyName(FOOT_LENGTH);
    c3.setKeyType(SizeChartHeaderType.DIMENSION.name());
    c3.setValue(FOOT_LENGTH);
    SizeChartDataColumn c4 = new SizeChartDataColumn();
    c4.setKeyName(SHOULDER_LENGTH);
    c4.setKeyType(SizeChartHeaderType.DIMENSION.name());
    c4.setMax(SHOULDER_LENGTH);
    c4.setMin(SHOULDER_LENGTH);
    sizeChartDataRow = new SizeChartDataRow();
    sizeChartDataRow.setRowNumber(1);
    sizeChartDataRow.setColumns(List.of(c1, c2, c3, c4));
    sizeChartRequest.setSizeChartRows(List.of(sizeChartDataRow));
    sizeChart = new SizeChart();
    sizeChart.setSizeChartCode(SIZE_CHART_CODE);
    sizeChart.setSizeChartRows(objectMapper.writeValueAsString(List.of(sizeChartDataRow)));
    sizeChart.setSizeAttributeValueTypes(
      objectMapper.writeValueAsString(List.of(INTERNATIONAL, KIDS)));
    sizeChart.setDimensions(
      objectMapper.writeValueAsString(List.of(DIMENSION_CODE_1, DIMENSION_CODE_2)));
    sizeChart.setName(SIZE_CHART_NAME);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(sizeChartRepository);
    Mockito.verifyNoMoreInteractions(mapper);
    Mockito.verifyNoMoreInteractions(attributeService);
    Mockito.verifyNoMoreInteractions(categoryService);
  }

  @Test
  public void filterTest() throws Exception {
    ReflectionTestUtils.setField(this.sizeChartServiceBean, "populateDescription", false);
    ReflectionTestUtils.setField(this.sizeChartServiceBean, "sortExternalSizeChartsByCreatedDateDescending", true);
    Mockito.when(
        sizeChartRepository.findByStoreIdAndFilterAppliedAndBusinessPartnerCodeAndMarkForDeleteFalse(
            STORE_ID, new SizeChartFilterRequestDTO(), PageRequest.of(0, 10), true)).thenReturn(sizeChartPage);
    Page<SizeChartResponse> sizeChartResponses =
        sizeChartServiceBean.filter(STORE_ID, sizeChartFilterRequest, PageRequest.of(0, 10));
    Mockito.verify(sizeChartRepository)
        .findByStoreIdAndFilterAppliedAndBusinessPartnerCodeAndMarkForDeleteFalse(STORE_ID, new SizeChartFilterRequestDTO(),
            PageRequest.of(0, 10), true);
    Assertions.assertEquals(1, sizeChartResponses.getTotalElements());
  }

  @Test
  void filterWithDescriptionTest() throws Exception {
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setDescription(DESCRIPTION.getBytes());
    ReflectionTestUtils.setField(this.sizeChartServiceBean, "populateDescription", true);
    ReflectionTestUtils.setField(this.sizeChartServiceBean, "sortExternalSizeChartsByCreatedDateDescending", true);
    Mockito.when(
      sizeChartRepository.findByStoreIdAndFilterAppliedAndBusinessPartnerCodeAndMarkForDeleteFalse(
        STORE_ID, new SizeChartFilterRequestDTO(), PageRequest.of(0, 10), true)).thenReturn(sizeChartPage);
    Mockito.when(attributeService.findDetailByStoreIdAndAttributeCodeList(STORE_ID,
      List.of(ATTRIBUTE_CODE))).thenReturn(List.of(attribute));
    Page<SizeChartResponse> sizeChartResponses =
      sizeChartServiceBean.filter(STORE_ID, sizeChartFilterRequest, PageRequest.of(0, 10));
    Mockito.verify(sizeChartRepository)
      .findByStoreIdAndFilterAppliedAndBusinessPartnerCodeAndMarkForDeleteFalse(STORE_ID, new SizeChartFilterRequestDTO(),
        PageRequest.of(0, 10), true);
    Mockito.verify(attributeService)
      .findDetailByStoreIdAndAttributeCodeList(STORE_ID, List.of(ATTRIBUTE_CODE));
    Assertions.assertEquals(1, sizeChartResponses.getTotalElements());
    Assertions.assertNotNull(sizeChartResponses.getContent().get(0).getDescription());
  }

  @Test
  public void upsertSizeChartCreateTest() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    Mockito.when(sizeChartRepository.getSequenceForSizeChart(PREFIX_SIZE_CHART_CODE))
      .thenReturn(1L);
    Mockito.when(mapper.writeValueAsString(sizeChartRequest.getSelectedValueTypes()))
      .thenReturn(objectMapper.writeValueAsString(sizeChartRequest.getSelectedValueTypes()));
    sizeChartServiceBean.upsertSizeChart(sizeChartRequest, StringUtils.EMPTY);
    Mockito.verify(sizeChartRepository).getSequenceForSizeChart(PREFIX_SIZE_CHART_CODE);
    Mockito.verify(sizeChartRepository).save(sizeChartArgumentCaptor.capture());
    Assertions.assertEquals(NAME, sizeChartArgumentCaptor.getValue().getName());
  }

  @Test
  public void upsertSizeChartEditTest() throws Exception {
    sizeChartRequest.setSizeChartCode(SIZE_CHART_CODE);
    ObjectMapper objectMapper = new ObjectMapper();
    Mockito.when(
      sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, SIZE_CHART_CODE,
        false)).thenReturn(new SizeChart());
    Mockito.when(mapper.writeValueAsString(sizeChartRequest.getSelectedValueTypes()))
      .thenReturn(objectMapper.writeValueAsString(sizeChartRequest.getSelectedValueTypes()));
    sizeChartServiceBean.upsertSizeChart(sizeChartRequest, STORE_ID);
    Mockito.verify(sizeChartRepository)
      .findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, SIZE_CHART_CODE, false);
    Mockito.verify(sizeChartRepository).save(sizeChartArgumentCaptor.capture());
    Assertions.assertEquals(NAME, sizeChartArgumentCaptor.getValue().getName());
  }
  @Test
  public void fetchSizeChartDetailTest() throws Exception {
    Mockito.when(
      sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, SIZE_CHART_CODE,
        false)).thenReturn(sizeChart);
    SizeChartDetailResponse response =
      sizeChartServiceBean.fetchSizeChartDetails(STORE_ID, SIZE_CHART_CODE);
    Mockito.verify(
      sizeChartRepository).findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, SIZE_CHART_CODE,
        false);
    Assertions.assertEquals(SIZE_CHART_CODE, response.getSizeChartCode());
    Assertions.assertEquals(sizeChartDataRow, response.getSizeChartRows().get(0));
  }

  @Test
  public void deleteSizeChartTest_markForDeleteTrue() {
    Mockito.when(
        sizeChartRepository.findByStoreIdAndSizeChartCodeAndBusinessPartnerCodeAndMarkForDelete(
            STORE_ID, SIZE_CHART_CODE, BUSINESS_PARTNER_CODE, false)).thenReturn(sizeChart);
    sizeChartServiceBean.updateSizeChartStatusBySizeChartCode(STORE_ID, SIZE_CHART_CODE,
        BUSINESS_PARTNER_CODE, null, true);
    Mockito.verify(sizeChartRepository)
        .findByStoreIdAndSizeChartCodeAndBusinessPartnerCodeAndMarkForDelete(STORE_ID,
            SIZE_CHART_CODE, BUSINESS_PARTNER_CODE, false);
    Mockito.verify(sizeChartRepository).save(sizeChart);
  }

  @Test
  public void deleteSizeChartTest_waitingDeletionTrue() {
    Mockito.when(
        sizeChartRepository.findByStoreIdAndSizeChartCodeAndBusinessPartnerCodeAndMarkForDelete(
            STORE_ID, SIZE_CHART_CODE, BUSINESS_PARTNER_CODE, false)).thenReturn(sizeChart);
    sizeChartServiceBean.updateSizeChartStatusBySizeChartCode(STORE_ID, SIZE_CHART_CODE,
        BUSINESS_PARTNER_CODE, true, null);
    Mockito.verify(sizeChartRepository)
        .findByStoreIdAndSizeChartCodeAndBusinessPartnerCodeAndMarkForDelete(STORE_ID,
            SIZE_CHART_CODE, BUSINESS_PARTNER_CODE, false);
    Mockito.verify(sizeChartRepository).save(sizeChart);
  }

  @Test
  public void findByNameAndBusinessPartnerCodeTest_success() throws Exception {
    Mockito.when(
        sizeChartRepository.findByStoreIdAndNameAndBusinessPartnerCodeAndMarkForDelete(STORE_ID,
            SIZE_CHART_NAME, BUSINESS_PARTNER_CODE, false)).thenReturn(sizeChart);
    SizeChartResponse response =
        sizeChartServiceBean.findByNameAndBusinessPartnerCode(STORE_ID, SIZE_CHART_NAME, BUSINESS_PARTNER_CODE);
    Mockito.verify(
        sizeChartRepository).findByStoreIdAndNameAndBusinessPartnerCodeAndMarkForDelete(STORE_ID,
        SIZE_CHART_NAME, BUSINESS_PARTNER_CODE, false);
    Assertions.assertEquals(SIZE_CHART_CODE, response.getSizeChartCode());
    Assertions.assertEquals(SIZE_CHART_NAME, response.getSizeChartName());
  }

  @Test
  public void findByNameAndBusinessPartnerCodeTest_nullResponse() throws Exception {
    Mockito.when(
        sizeChartRepository.findByStoreIdAndNameAndBusinessPartnerCodeAndMarkForDelete(STORE_ID,
            SIZE_CHART_NAME, BUSINESS_PARTNER_CODE, false)).thenReturn(null);
    SizeChartResponse response =
        sizeChartServiceBean.findByNameAndBusinessPartnerCode(STORE_ID, SIZE_CHART_NAME, BUSINESS_PARTNER_CODE);
    Mockito.verify(
        sizeChartRepository).findByStoreIdAndNameAndBusinessPartnerCodeAndMarkForDelete(STORE_ID,
        SIZE_CHART_NAME, BUSINESS_PARTNER_CODE, false);
    Assertions.assertNull(response);
  }

  @Test
  public void validateCategoryAttributesForSizeChartNotCnTest() throws Exception {
    boolean response =
        sizeChartServiceBean.validateCategoryAttributesForSizeChart(STORE_ID, SIZE_CHART_CODE, CATEGORY_CODE);
    Mockito.verify(categoryService).validateIsCategoryCn(STORE_ID, CATEGORY_CODE);
    Assertions.assertFalse(response);
  }

  @Test
  public void validateCategoryAttributesForSizeChartNotValidTest() throws Exception {
    Mockito.when(categoryService.validateIsCategoryCn(STORE_ID, CATEGORY_CODE)).thenReturn(true);
    Mockito.when(attributeService.getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(new ArrayList<>());
    Mockito.when(sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, SIZE_CHART_CODE, false))
        .thenReturn(sizeChart);
    boolean response =
        sizeChartServiceBean.validateCategoryAttributesForSizeChart(STORE_ID, SIZE_CHART_CODE, CATEGORY_CODE);
    Mockito.verify(categoryService).validateIsCategoryCn(STORE_ID, CATEGORY_CODE);
    Mockito.verify(attributeService).getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(sizeChartRepository).findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, SIZE_CHART_CODE, false);
    Assertions.assertFalse(response);
  }

  @Test
  public void validateCategoryAttributesForSizeChartNotSizeAttributeTest() throws Exception {
    sizeChart.setSizeAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(categoryService.validateIsCategoryCn(STORE_ID, CATEGORY_CODE)).thenReturn(true);
    AttributeBasicDetailDTO attributeBasicDetailDTO = new AttributeBasicDetailDTO();
    attributeBasicDetailDTO.setAttributeCode(sizeChart.getSizeAttributeCode());
    Mockito.when(attributeService.getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(attributeBasicDetailDTO));
    Mockito.when(sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, SIZE_CHART_CODE, false))
        .thenReturn(sizeChart);
    boolean response =
        sizeChartServiceBean.validateCategoryAttributesForSizeChart(STORE_ID, SIZE_CHART_CODE, CATEGORY_CODE);
    Mockito.verify(categoryService).validateIsCategoryCn(STORE_ID, CATEGORY_CODE);
    Mockito.verify(attributeService).getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(sizeChartRepository).findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, SIZE_CHART_CODE, false);
    Assertions.assertFalse(response);
  }

  @Test
  public void validateCategoryAttributesForSizeChartSizeAttributeTest() throws Exception {
    sizeChart.setSizeAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(categoryService.validateIsCategoryCn(STORE_ID, CATEGORY_CODE)).thenReturn(true);
    AttributeBasicDetailDTO attributeBasicDetailDTO = new AttributeBasicDetailDTO();
    attributeBasicDetailDTO.setAttributeCode(STORE_ID);
    Mockito.when(attributeService.getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(attributeBasicDetailDTO));
    Mockito.when(sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, SIZE_CHART_CODE, false))
        .thenReturn(sizeChart);
    boolean response =
        sizeChartServiceBean.validateCategoryAttributesForSizeChart(STORE_ID, SIZE_CHART_CODE, CATEGORY_CODE);
    Mockito.verify(categoryService).validateIsCategoryCn(STORE_ID, CATEGORY_CODE);
    Mockito.verify(attributeService).getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(sizeChartRepository).findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, SIZE_CHART_CODE, false);
    Assertions.assertFalse(response);
  }

  @Test
  public void validateCategoryAttributesForSizeChartValidTest() throws Exception {
    sizeChart.setSizeAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(categoryService.validateIsCategoryCn(STORE_ID, CATEGORY_CODE)).thenReturn(true);
    AttributeBasicDetailDTO attributeBasicDetailDTO = new AttributeBasicDetailDTO();
    attributeBasicDetailDTO.setSizeAttribute(true);
    attributeBasicDetailDTO.setAttributeCode(sizeChart.getSizeAttributeCode());
    Mockito.when(attributeService.getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(attributeBasicDetailDTO));
    Mockito.when(sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, SIZE_CHART_CODE, false))
        .thenReturn(sizeChart);
    boolean response =
        sizeChartServiceBean.validateCategoryAttributesForSizeChart(STORE_ID, SIZE_CHART_CODE, CATEGORY_CODE);
    Mockito.verify(categoryService).validateIsCategoryCn(STORE_ID, CATEGORY_CODE);
    Mockito.verify(attributeService).getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(sizeChartRepository).findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, SIZE_CHART_CODE, false);
    Assertions.assertTrue(response);
  }

  @Test
  public void findSizeChartsBySizeChartCodeTest() {
    SizeChart sizeChart = new SizeChart();
    sizeChart.setSizeChartCode(SIZE_CHART_CODE);
    sizeChart.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    sizeChart.setName(SIZE_CHART_NAME);
    Mockito.when(sizeChartRepository.findByStoreIdAndSizeChartCodeInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(SIZE_CHART_CODE))).thenReturn(Arrays.asList(sizeChart));
    List<SizeChart> sizeCharts = sizeChartServiceBean.findSizeChartsBySizeChartCode(STORE_ID,
        Arrays.asList(SIZE_CHART_CODE));
    Mockito.verify(sizeChartRepository)
        .findByStoreIdAndSizeChartCodeInAndMarkForDeleteFalse(STORE_ID,
            Arrays.asList(SIZE_CHART_CODE));
    Assertions.assertEquals(SIZE_CHART_NAME, sizeCharts.get(0).getName());
  }

  @Test
  public void validateSizeChartForBusinessPartnerCodeAndCategoryCode_Success() throws Exception {
    // Given
    String sizeChartCode = "SIZE_CHART_001";
    String attributeCode = "SIZE_ATTR_001";
    String businessPartnerCode = "BP_001";
    
    SizeChart sizeChart = new SizeChart();
    sizeChart.setSizeChartCode(sizeChartCode);
    sizeChart.setBusinessPartnerCode(businessPartnerCode);
    sizeChart.setSizeAttributeCode(attributeCode);
    
    AttributeBasicDetailDTO attributeDetail = new AttributeBasicDetailDTO();
    attributeDetail.setAttributeCode(attributeCode);
    attributeDetail.setSizeAttribute(true);
    
    Mockito.when(sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false))
        .thenReturn(sizeChart);
    Mockito.when(attributeService.getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(List.of(attributeDetail));
    
    // When & Then (should not throw exception)
    sizeChartServiceBean.validateSizeChartForBusinessPartnerCodeAndCategoryCode(
        STORE_ID, sizeChartCode, businessPartnerCode, CATEGORY_CODE);
    
    // Verify
    Mockito.verify(sizeChartRepository).findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false);
    Mockito.verify(attributeService).getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void validateSizeChartForBusinessPartnerCodeAndCategoryCode_InternalBusinessPartner_Success() throws Exception {
    // Given
    String sizeChartCode = "SIZE_CHART_001";
    String attributeCode = "SIZE_ATTR_001";
    
    SizeChart sizeChart = new SizeChart();
    sizeChart.setSizeChartCode(sizeChartCode);
    sizeChart.setBusinessPartnerCode("INTERNAL");
    sizeChart.setSizeAttributeCode(attributeCode);
    
    AttributeBasicDetailDTO attributeDetail = new AttributeBasicDetailDTO();
    attributeDetail.setAttributeCode(attributeCode);
    attributeDetail.setSizeAttribute(true);
    
    Mockito.when(sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false))
        .thenReturn(sizeChart);
    Mockito.when(attributeService.getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(List.of(attributeDetail));
    
    // When & Then (should not throw exception)
    sizeChartServiceBean.validateSizeChartForBusinessPartnerCodeAndCategoryCode(
        STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    
    // Verify
    Mockito.verify(sizeChartRepository).findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false);
    Mockito.verify(attributeService).getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void validateSizeChartForBusinessPartnerCodeAndCategoryCode_BlankCategoryCode_ThrowsException() {
    // Given
    String sizeChartCode = "SIZE_CHART_001";
    String blankCategoryCode = "";
    
    // When & Then
    ValidationException exception = assertThrows(ValidationException.class, () -> {
      sizeChartServiceBean.validateSizeChartForBusinessPartnerCodeAndCategoryCode(
          STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, blankCategoryCode);
    });
    
    // Verify no service calls were made (due to early validation)
    // Note: No verification needed as the method should fail before making any calls
  }

  @Test
  public void validateSizeChartForBusinessPartnerCodeAndCategoryCode_SizeChartNotFound_ThrowsException() throws Exception {
    // Given
    String sizeChartCode = "SIZE_CHART_001";
    
    // Mock the repository to return null, which will cause findBySizeChartCodeAndMarkForDeleteFalse to throw ValidationException
    Mockito.when(sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false))
        .thenReturn(null);
    
    // When & Then
    ValidationException exception = assertThrows(ValidationException.class, () -> {
      sizeChartServiceBean.validateSizeChartForBusinessPartnerCodeAndCategoryCode(
          STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    });
    
    // Verify
    Mockito.verify(sizeChartRepository).findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false);
  }

  @Test
  public void validateSizeChartForBusinessPartnerCodeAndCategoryCode_BusinessPartnerMismatch_ThrowsException() throws Exception {
    // Given
    String sizeChartCode = "SIZE_CHART_001";
    String differentBusinessPartnerCode = "DIFFERENT_BP";
    
    SizeChart sizeChart = new SizeChart();
    sizeChart.setSizeChartCode(sizeChartCode);
    sizeChart.setBusinessPartnerCode(differentBusinessPartnerCode);
    
    Mockito.when(sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false))
        .thenReturn(sizeChart);
    
    // When & Then
    ValidationException exception = assertThrows(ValidationException.class, () -> {
      sizeChartServiceBean.validateSizeChartForBusinessPartnerCodeAndCategoryCode(
          STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    });
    
    // Verify
    Mockito.verify(sizeChartRepository).findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false);
  }

  @Test
  public void validateSizeChartForBusinessPartnerCodeAndCategoryCode_AttributeNotFound_ThrowsException() throws Exception {
    // Given
    String sizeChartCode = "SIZE_CHART_001";
    String attributeCode = "SIZE_ATTR_001";
    String differentAttributeCode = "DIFFERENT_ATTR";
    
    SizeChart sizeChart = new SizeChart();
    sizeChart.setSizeChartCode(sizeChartCode);
    sizeChart.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    sizeChart.setSizeAttributeCode(attributeCode);
    
    AttributeBasicDetailDTO attributeDetail = new AttributeBasicDetailDTO();
    attributeDetail.setAttributeCode(differentAttributeCode); // Different attribute code
    attributeDetail.setSizeAttribute(true);
    
    Mockito.when(sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false))
        .thenReturn(sizeChart);
    Mockito.when(attributeService.getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(List.of(attributeDetail));
    
    // When & Then
    ValidationException exception = assertThrows(ValidationException.class, () -> {
      sizeChartServiceBean.validateSizeChartForBusinessPartnerCodeAndCategoryCode(
          STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    });
    
    // Verify
    Mockito.verify(sizeChartRepository).findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false);
    Mockito.verify(attributeService).getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void validateSizeChartForBusinessPartnerCodeAndCategoryCode_AttributeNotSizeAttribute_ThrowsException() throws Exception {
    // Given
    String sizeChartCode = "SIZE_CHART_001";
    String attributeCode = "SIZE_ATTR_001";
    
    SizeChart sizeChart = new SizeChart();
    sizeChart.setSizeChartCode(sizeChartCode);
    sizeChart.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    sizeChart.setSizeAttributeCode(attributeCode);
    
    AttributeBasicDetailDTO attributeDetail = new AttributeBasicDetailDTO();
    attributeDetail.setAttributeCode(attributeCode);
    attributeDetail.setSizeAttribute(false); // Not a size attribute
    
    Mockito.when(sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false))
        .thenReturn(sizeChart);
    Mockito.when(attributeService.getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(List.of(attributeDetail));
    
    // When & Then
    ValidationException exception = assertThrows(ValidationException.class, () -> {
      sizeChartServiceBean.validateSizeChartForBusinessPartnerCodeAndCategoryCode(
          STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    });
    
    // Verify
    Mockito.verify(sizeChartRepository).findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false);
    Mockito.verify(attributeService).getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void validateSizeChartForBusinessPartnerCodeAndCategoryCode_EmptyAttributeList_ThrowsException() throws Exception {
    // Given
    String sizeChartCode = "SIZE_CHART_001";
    String attributeCode = "SIZE_ATTR_001";
    
    SizeChart sizeChart = new SizeChart();
    sizeChart.setSizeChartCode(sizeChartCode);
    sizeChart.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    sizeChart.setSizeAttributeCode(attributeCode);
    
    Mockito.when(sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false))
        .thenReturn(sizeChart);
    Mockito.when(attributeService.getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(new ArrayList<>()); // Empty list
    
    // When & Then
    ValidationException exception = assertThrows(ValidationException.class, () -> {
      sizeChartServiceBean.validateSizeChartForBusinessPartnerCodeAndCategoryCode(
          STORE_ID, sizeChartCode, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    });
    
    // Verify
    Mockito.verify(sizeChartRepository).findByStoreIdAndSizeChartCodeAndMarkForDelete(STORE_ID, sizeChartCode, false);
    Mockito.verify(attributeService).getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE);
  }
}
