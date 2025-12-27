package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.util.BeanUtils;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.dto.Gender;
import com.gdn.x.productcategorybase.dto.SizeChartHeaderType;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.SizeChartDataColumn;
import com.gdn.x.productcategorybase.dto.request.SizeChartDataRow;
import com.gdn.x.productcategorybase.dto.request.SizeChartRequest;
import com.gdn.x.productcategorybase.dto.response.SizeChartDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SizeChartResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Dimension;
import com.gdn.x.productcategorybase.entity.DimensionMapping;
import com.gdn.x.productcategorybase.entity.SizeChart;
import com.gdn.x.productcategorybase.exception.ValidationException;
import com.gdn.x.productcategorybase.service.DimensionMappingService;
import com.gdn.x.productcategorybase.service.MasterAttributeService;
import com.gdn.x.productcategorybase.service.SizeChartService;
import com.gdn.x.productcategorybase.service.brand.BrandService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import org.springframework.test.util.ReflectionTestUtils;

public class SizeChartWrapperServiceBeanTest {

  private static final String STORE_ID = "10001";
  private static final String ATTRIBUTE_CODE = "attribute-code";
  private static final String SIZE_CHART_CODE = "size-chart-code";
  private static final String SIZE_CHART_NAME = "sizeChartName";
  private static final String BRAND_CODE = "brand-code";
  private static final String NAME = "sizeChartName";
  private static final String BRAND = "brand";
  private static final String NEW_BRAND_CODE = "newBrandCode";
  private static final String INTERNATIONAL = "International";
  private static final String KIDS = "Kids";
  private static final String DIMENSION_CODE_1 = "DIM-000001";
  private static final String DIMENSION_CODE_2 = "DIM-000002";
  private static final String FOOT_LENGTH = "Foot length";
  private static final String VALUE = "1.23";
  private static final String MIN_VALUE = "1.11";
  private static final String MAX_VALUE = "2.11";
  private static final String INVALID = "INVALID";
  private static final String SHOULDER_LENGTH = "Shoulder length";
  private static final String UNIT = "CM";
  private static final String BUSINESS_PARTNER_CODE = "INTERNAL";
  private static final String BRAND_NAME = "brandName";
  private static final String ATTRIBUTE_NAME = "attributeName";
  private static final String SIZE_CHART_UPDATE_EVENT_NAME = "sizeChartUpdateEventName";

  @InjectMocks
  private SizeChartWrapperServiceBean sizeChartWrapperServiceBean;

  @Mock
  private SizeChartService sizeChartService;

  @Mock
  private BrandService brandService;

  @Mock
  private DimensionMappingService dimensionMappingService;

  @Mock
  private MasterAttributeService masterAttributeService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private KafkaPublisher kafkaProducer;

  private Attribute attribute;
  private BrandResponse brandResponse;
  private SizeChartRequest sizeChartRequest;
  private SizeChartDataRow sizeChartDataRow;
  private DimensionMapping dimensionMapping1;
  private DimensionMapping dimensionMapping2;
  private SizeChartDetailResponse sizeChartDetailResponse;
  private ObjectMapper objectMapper = new ObjectMapper();
  private SizeChart sizeChart = new SizeChart();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    sizeChart.setName(SIZE_CHART_NAME);
    sizeChart.setSizeChartCode(SIZE_CHART_CODE);
    attribute = new Attribute();
    attribute.setSizeAttribute(true);
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setValueTypes(objectMapper.writeValueAsString(List.of(INTERNATIONAL, KIDS)));
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setAttributeImageUrl(INTERNATIONAL);
    brandResponse = new BrandResponse();
    brandResponse.setBrandCode(BRAND_CODE);
    brandResponse.setBrandName(BRAND_NAME);
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
    dimensionMapping1 = new DimensionMapping();
    Dimension dimension1 = new Dimension();
    dimension1.setDimensionCode(DIMENSION_CODE_1);
    dimension1.setName(FOOT_LENGTH);
    dimensionMapping1.setDimension(dimension1);

    dimensionMapping2 = new DimensionMapping();
    Dimension dimension2 = new Dimension();
    dimension2.setDimensionCode(DIMENSION_CODE_2);
    dimension2.setName(SHOULDER_LENGTH);
    dimensionMapping2.setDimension(dimension2);

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
    c3.setValue(VALUE);
    SizeChartDataColumn c4 = new SizeChartDataColumn();
    c4.setKeyName(SHOULDER_LENGTH);
    c4.setKeyType(SizeChartHeaderType.DIMENSION.name());
    c4.setMax(MAX_VALUE);
    c4.setMin(MIN_VALUE);
    sizeChartDataRow = new SizeChartDataRow();
    sizeChartDataRow.setRowNumber(1);
    sizeChartDataRow.setColumns(List.of(c1, c2, c3, c4));
    sizeChartRequest.setSizeChartRows(List.of(sizeChartDataRow));
    sizeChartDetailResponse=new SizeChartDetailResponse();
    sizeChartDetailResponse.setSizeChartCode(SIZE_CHART_CODE);
    sizeChartDetailResponse.setSizeAttributeCode(ATTRIBUTE_CODE);
    ReflectionTestUtils.setField(sizeChartWrapperServiceBean, "sizeChartValidUnits", "INCH,CM");
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(sizeChartService);
    Mockito.verifyNoMoreInteractions(brandService);
    Mockito.verifyNoMoreInteractions(masterAttributeService);
    Mockito.verifyNoMoreInteractions(dimensionMappingService);
  }

  @Test
  public void upsertSizeChartTest() throws Exception {
    sizeChartRequest.setName("sizeChartName");
    sizeChartRequest.setSizeChartCode(SIZE_CHART_CODE);
    SizeChart sizeChart = new SizeChart();
    BeanUtils.copyProperties(sizeChartRequest, sizeChart);
    Mockito.when(kafkaTopicProperties.getSizeChartUpdateEventName()).thenReturn(SIZE_CHART_UPDATE_EVENT_NAME);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
        .thenReturn(brandResponse);
    Mockito.when(sizeChartService.upsertSizeChart(sizeChartRequest, STORE_ID)).thenReturn(sizeChart);
    Mockito.when(
            masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
        .thenReturn(attribute);
    Mockito.when(dimensionMappingService.fetchDimensionMappingForAttribute(STORE_ID,
        attribute.getAttributeCode())).thenReturn(List.of(dimensionMapping1, dimensionMapping2));
    Mockito.when(
        sizeChartService.findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode())).thenReturn(null);
    sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest);
    Mockito.verify(sizeChartService).upsertSizeChart(sizeChartRequest, STORE_ID);
    Mockito.verify(brandService).findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode());
    Mockito.verify(masterAttributeService)
        .findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode());
    Mockito.verify(dimensionMappingService)
        .fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode());
    Mockito.verify(sizeChartService).upsertSizeChart(sizeChartRequest, STORE_ID);
    Mockito.verify(sizeChartService)
        .findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode());
    Mockito.verify(kafkaTopicProperties).getSizeChartUpdateEventName();
    Mockito.verify(kafkaProducer).send(Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void upsertSizeChartTest_nonNumericDimensionValue() throws Exception {
    sizeChartRequest.setName("sizeChartName");
    sizeChartRequest.setSizeChartCode(SIZE_CHART_CODE);
    sizeChartRequest.getSizeChartRows().get(0).getColumns().get(2).setValue(INVALID);
    SizeChart sizeChart = new SizeChart();
    BeanUtils.copyProperties(sizeChartRequest, sizeChart);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
        .thenReturn(brandResponse);
    Mockito.when(
            masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
        .thenReturn(attribute);
    Mockito.when(dimensionMappingService.fetchDimensionMappingForAttribute(STORE_ID,
        attribute.getAttributeCode())).thenReturn(List.of(dimensionMapping1, dimensionMapping2));
    try {
      sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest);
    }
    catch (ValidationException ex) {
      Assertions.assertEquals(
          ErrorMessage.DIMENSION_VALUE_CAN_ONLY_HAVE_NUMERIC_VALUES_ERROR_CODE.getMessage(),
          ex.getErrorCode());
      Assertions.assertEquals(
          ErrorMessage.DIMENSION_VALUE_CAN_ONLY_HAVE_NUMERIC_VALUES.getMessage(),
          ex.getErrorMessage());
    }
    Mockito.verify(brandService).findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode());
    Mockito.verify(masterAttributeService)
        .findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode());
    Mockito.verify(dimensionMappingService)
        .fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode());
    Mockito.verify(sizeChartService)
        .findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode());
  }

  @Test
  public void upsertSizeChartTest_dimensionValueCanEitherBeSingleOrRange() throws Exception {
    sizeChartRequest.setName("sizeChartName");
    sizeChartRequest.setSizeChartCode(SIZE_CHART_CODE);
    sizeChartRequest.getSizeChartRows().get(0).getColumns().get(2).setMax(MAX_VALUE);
    SizeChart sizeChart = new SizeChart();
    BeanUtils.copyProperties(sizeChartRequest, sizeChart);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
        .thenReturn(brandResponse);
    Mockito.when(
            masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
        .thenReturn(attribute);
    Mockito.when(dimensionMappingService.fetchDimensionMappingForAttribute(STORE_ID,
        attribute.getAttributeCode())).thenReturn(List.of(dimensionMapping1, dimensionMapping2));
    try {
      sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest);
    }
    catch (ValidationException ex) {
      Assertions.assertEquals(
          ErrorMessage.DIMENSION_VALUE_CAN_EITHER_BE_SINGLE_OR_RANGE_ERROR_CODE.getMessage(),
          ex.getErrorCode());
      Assertions.assertEquals(
          ErrorMessage.DIMENSION_VALUE_CAN_EITHER_BE_SINGLE_OR_RANGE.getMessage(),
          ex.getErrorMessage());
    }
    Mockito.verify(brandService).findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode());
    Mockito.verify(masterAttributeService)
        .findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode());
    Mockito.verify(dimensionMappingService)
        .fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode());
    Mockito.verify(sizeChartService)
        .findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode());
  }

  @Test
  public void upsertSizeChartTest_maxValueShouldBeGreaterThanMinvalue() throws Exception {
    sizeChartRequest.setName("sizeChartName");
    sizeChartRequest.setSizeChartCode(SIZE_CHART_CODE);
    sizeChartRequest.getSizeChartRows().get(0).getColumns().get(2).setValue(null);
    sizeChartRequest.getSizeChartRows().get(0).getColumns().get(2).setMax(MAX_VALUE);
    sizeChartRequest.getSizeChartRows().get(0).getColumns().get(2).setMin(MAX_VALUE);
    SizeChart sizeChart = new SizeChart();
    BeanUtils.copyProperties(sizeChartRequest, sizeChart);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
        .thenReturn(brandResponse);
    Mockito.when(
            masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
        .thenReturn(attribute);
    Mockito.when(dimensionMappingService.fetchDimensionMappingForAttribute(STORE_ID,
        attribute.getAttributeCode())).thenReturn(List.of(dimensionMapping1, dimensionMapping2));
    try {
      sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest);
    }
    catch (ValidationException ex) {
      Assertions.assertEquals(
          ErrorMessage.MAX_VALUE_SHOULD_BE_GREATER_THAN_MIN_VALUE_ERROR_CODE.getMessage(),
          ex.getErrorCode());
      Assertions.assertEquals(
          ErrorMessage.MAX_VALUE_SHOULD_BE_GREATER_THAN_MIN_VALUE.getMessage(),
          ex.getErrorMessage());
    }
    Mockito.verify(brandService).findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode());
    Mockito.verify(masterAttributeService)
        .findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode());
    Mockito.verify(dimensionMappingService)
        .fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode());
    Mockito.verify(sizeChartService)
        .findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode());
  }

  @Test
  public void upsertSizeChartTest_invalidUnit() throws Exception {
    sizeChartRequest.setName("sizeChartName");
    sizeChartRequest.setSizeChartCode(SIZE_CHART_CODE);
    sizeChartRequest.setUnit(INVALID);
    SizeChart sizeChart = new SizeChart();
    BeanUtils.copyProperties(sizeChartRequest, sizeChart);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
        .thenReturn(brandResponse);
    Mockito.when(
            masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
        .thenReturn(attribute);
    Mockito.when(dimensionMappingService.fetchDimensionMappingForAttribute(STORE_ID,
        attribute.getAttributeCode())).thenReturn(List.of(dimensionMapping1, dimensionMapping2));
    try {
      sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest);
    }
    catch (ValidationException ex) {
      Assertions.assertEquals(
          ErrorMessage.SIZE_CHART_INVALID_UNIT_ERROR_CODE.getMessage(),
          ex.getErrorCode());
      Assertions.assertEquals(
          ErrorMessage.SIZE_CHART_INVALID_UNIT.getMessage(),
          ex.getErrorMessage());
    }
    Mockito.verify(brandService).findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode());
    Mockito.verify(masterAttributeService)
        .findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode());
    Mockito.verify(dimensionMappingService)
        .fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode());
    Mockito.verify(sizeChartService)
        .findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode());
  }

  @Test
  public void upsertSizeChartHeaderMismatchExceptionTest() throws Exception {
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
      .thenReturn(brandResponse);
    Mockito.when(
        masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
      .thenReturn(attribute);
    Mockito.when(dimensionMappingService.fetchDimensionMappingForAttribute(STORE_ID,
      attribute.getAttributeCode())).thenReturn(List.of(dimensionMapping1, dimensionMapping2));
    Mockito.when(
        sizeChartService.findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode())).thenReturn(null);
    sizeChartRequest.getSelectedDimensionCodes()
      .remove(sizeChartRequest.getSelectedDimensionCodes().size() - 1);
    try {
      Assertions.assertThrows(ValidationException.class, () -> sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest));
    }finally {
      Mockito.verify(brandService).findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode());
      Mockito.verify(masterAttributeService)
        .findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode());
      Mockito.verify(dimensionMappingService)
        .fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode());
      Mockito.verify(sizeChartService)
          .findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
              sizeChartRequest.getBusinessPartnerCode());
    }
  }

  @Test
  public void upsertSizeChartValueEmptyTest() throws Exception {
    sizeChartRequest.getSizeChartRows().get(0).getColumns().get(2).setValue(StringUtils.EMPTY);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
      .thenReturn(brandResponse);
    Mockito.when(
        masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
      .thenReturn(attribute);
    Mockito.when(dimensionMappingService.fetchDimensionMappingForAttribute(STORE_ID,
      attribute.getAttributeCode())).thenReturn(List.of(dimensionMapping1, dimensionMapping2));
    Mockito.when(
        sizeChartService.findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode())).thenReturn(null);
    try {
      Assertions.assertThrows(ValidationException.class, () -> sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest));
    }finally {
      Mockito.verify(brandService).findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode());
      Mockito.verify(masterAttributeService)
        .findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode());
      Mockito.verify(dimensionMappingService)
        .fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode());
      Mockito.verify(sizeChartService)
          .findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
              sizeChartRequest.getBusinessPartnerCode());
    }
  }

  @Test
  public void upsertSizeChartRangeEmptyTest() throws Exception {
    sizeChartRequest.getSizeChartRows().get(0).getColumns().get(3).setMin(StringUtils.EMPTY);
    sizeChartRequest.getSizeChartRows().get(0).getColumns().get(3).setMax(StringUtils.EMPTY);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
      .thenReturn(brandResponse);
    Mockito.when(
        masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
      .thenReturn(attribute);
    Mockito.when(dimensionMappingService.fetchDimensionMappingForAttribute(STORE_ID,
      attribute.getAttributeCode())).thenReturn(List.of(dimensionMapping1, dimensionMapping2));
    Mockito.when(
        sizeChartService.findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode())).thenReturn(null);
    try {
      Assertions.assertThrows(ValidationException.class, () -> sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest));
    }finally {
      Mockito.verify(brandService).findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode());
      Mockito.verify(masterAttributeService)
        .findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode());
      Mockito.verify(dimensionMappingService)
        .fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode());
      Mockito.verify(sizeChartService)
          .findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
              sizeChartRequest.getBusinessPartnerCode());
    }
  }

  @Test
  public void upsertSizeChartRangeEmptyTest1() throws Exception {
    sizeChartRequest.getSizeChartRows().get(0).getColumns().get(3).setMin(StringUtils.EMPTY);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
      .thenReturn(brandResponse);
    Mockito.when(
        masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
      .thenReturn(attribute);
    Mockito.when(dimensionMappingService.fetchDimensionMappingForAttribute(STORE_ID,
      attribute.getAttributeCode())).thenReturn(List.of(dimensionMapping1, dimensionMapping2));
    Mockito.when(
        sizeChartService.findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode())).thenReturn(null);
    try {
      Assertions.assertThrows(ValidationException.class, () -> sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest));
    }finally {
      Mockito.verify(brandService).findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode());
      Mockito.verify(masterAttributeService)
        .findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode());
      Mockito.verify(dimensionMappingService)
        .fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode());
      Mockito.verify(sizeChartService)
          .findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
              sizeChartRequest.getBusinessPartnerCode());
    }
  }

  @Test
  public void upsertSizeChartRangeEmptyTest2() throws Exception {
    sizeChartRequest.getSizeChartRows().get(0).getColumns().get(3).setMax(StringUtils.EMPTY);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
      .thenReturn(brandResponse);
    Mockito.when(
        masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
      .thenReturn(attribute);
    Mockito.when(dimensionMappingService.fetchDimensionMappingForAttribute(STORE_ID,
      attribute.getAttributeCode())).thenReturn(List.of(dimensionMapping1, dimensionMapping2));
    Mockito.when(
        sizeChartService.findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode())).thenReturn(null);
    try {
      Assertions.assertThrows(ValidationException.class, () -> sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest));
    }finally {
      Mockito.verify(brandService).findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode());
      Mockito.verify(masterAttributeService)
        .findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode());
      Mockito.verify(dimensionMappingService)
        .fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode());
      Mockito.verify(sizeChartService)
          .findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
              sizeChartRequest.getBusinessPartnerCode());
    }
  }

  @Test
  public void upsertSizeChartValueTypesEmptyTest() throws Exception {
    attribute.setValueTypes(StringUtils.EMPTY);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
      .thenReturn(brandResponse);
    Mockito.when(
        masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
      .thenReturn(attribute);
    Mockito.when(
        sizeChartService.findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode())).thenReturn(null);
    try {
      Assertions.assertThrows(ValidationException.class, () -> sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest));
    }finally {
      Mockito.verify(brandService).findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode());
      Mockito.verify(masterAttributeService)
        .findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode());
      Mockito.verify(sizeChartService)
          .findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
              sizeChartRequest.getBusinessPartnerCode());
    }
  }

  @Test
  public void upsertSizeChartDuplicateSizeChartTest() throws Exception {
    attribute.setValueTypes(StringUtils.EMPTY);
    Mockito.when(
        sizeChartService.findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode())).thenReturn(new SizeChartResponse());
    try {
      sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest);
    } catch (Exception e) {
      if (e instanceof ValidationException) {
        ValidationException v = (ValidationException) e;
        Assertions.assertEquals(ErrorMessage.SIZE_CHART_NAME_ALREADY_EXISTS.getMessage(),
            v.getErrorMessage());
      }
    } finally {
      Mockito.verify(sizeChartService)
          .findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
              sizeChartRequest.getBusinessPartnerCode());
    }
  }
  @Test
  public void fetchSizeChartDetailsTest() throws Exception {
    Mockito.when(sizeChartService.fetchSizeChartDetails(STORE_ID, SIZE_CHART_CODE))
      .thenReturn(sizeChartDetailResponse);
    Mockito.when(masterAttributeService.findDetailByAttributeCode(ATTRIBUTE_CODE))
      .thenReturn(attribute);
    SizeChartDetailResponse detailResponse =
      sizeChartWrapperServiceBean.fetchSizeChartDetails(STORE_ID, true, SIZE_CHART_CODE);
    Mockito.verify(sizeChartService).fetchSizeChartDetails(STORE_ID, SIZE_CHART_CODE);
    Mockito.verify(masterAttributeService).findDetailByAttributeCode(ATTRIBUTE_CODE);
    Assertions.assertEquals(INTERNATIONAL, detailResponse.getAttributeImageUrl());
  }
  @Test
  public void fetchSizeChartDetailsWithoutImageTest() throws Exception {
    Mockito.when(sizeChartService.fetchSizeChartDetails(STORE_ID, SIZE_CHART_CODE))
      .thenReturn(sizeChartDetailResponse);
    SizeChartDetailResponse detailResponse =
      sizeChartWrapperServiceBean.fetchSizeChartDetails(STORE_ID, false, SIZE_CHART_CODE);
    Mockito.verify(sizeChartService).fetchSizeChartDetails(STORE_ID, SIZE_CHART_CODE);
    Assertions.assertNull(detailResponse.getAttributeImageUrl());
  }


  @Test
  public void upsertSizeChartTest1() throws Exception {
    Mockito.when(
        sizeChartService.findBySizeChartCodeAndBusinessPartnerCodeAndMarkForDeleteFalse(STORE_ID, SIZE_CHART_CODE,
            "DR6-44662")).thenReturn(sizeChart);
    sizeChartRequest.setName("sizeChartNameEdited");
    sizeChartRequest.setSizeChartCode(SIZE_CHART_CODE);
    Mockito.when(kafkaTopicProperties.getSizeChartUpdateEventName()).thenReturn(SIZE_CHART_UPDATE_EVENT_NAME);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
        .thenReturn(brandResponse);
    Mockito.when(sizeChartService.upsertSizeChart(sizeChartRequest, STORE_ID)).thenReturn(sizeChart);
    Mockito.when(masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
        .thenReturn(attribute);
    Mockito.when(sizeChartService.findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
        sizeChartRequest.getBusinessPartnerCode())).thenReturn(null);
    Mockito.when(dimensionMappingService.fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode()))
        .thenReturn(List.of(dimensionMapping1, dimensionMapping2));
    sizeChartRequest.setBusinessPartnerCode("DR6-44662");
    try {
     Assertions.assertThrows(RuntimeException.class, () -> sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest));
    } finally {
      Mockito.verify(sizeChartService).findByNameAndBusinessPartnerCode(STORE_ID, "sizeChartNameEdited",
          sizeChartRequest.getBusinessPartnerCode());
      Mockito.verify(sizeChartService)
          .findBySizeChartCodeAndBusinessPartnerCodeAndMarkForDeleteFalse(STORE_ID, SIZE_CHART_CODE, "DR6-44662");
    }
  }

  @Test
  public void upsertSizeChartTest3() throws Exception {
    Mockito.when(
        sizeChartService.findBySizeChartCodeAndBusinessPartnerCodeAndMarkForDeleteFalse(STORE_ID, SIZE_CHART_CODE,
            "DR6-44662")).thenReturn(sizeChart);
    sizeChartRequest.setName("sizeChartNameEdited");
    Mockito.when(kafkaTopicProperties.getSizeChartUpdateEventName()).thenReturn(SIZE_CHART_UPDATE_EVENT_NAME);
    Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
        .thenReturn(brandResponse);
    Mockito.when(sizeChartService.upsertSizeChart(sizeChartRequest, STORE_ID)).thenReturn(sizeChart);
    Mockito.when(masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
        .thenReturn(attribute);
    Mockito.when(sizeChartService.findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
        sizeChartRequest.getBusinessPartnerCode())).thenReturn(null);
    Mockito.when(dimensionMappingService.fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode()))
        .thenReturn(List.of(dimensionMapping1, dimensionMapping2));
    sizeChartRequest.setBusinessPartnerCode("DR6-44662");
    try {
      sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest);
    } finally {
      Mockito.verify(sizeChartService)
          .findByNameAndBusinessPartnerCode(STORE_ID, "sizeChartNameEdited", sizeChartRequest.getBusinessPartnerCode());
      Mockito.verify(sizeChartService).upsertSizeChart(Mockito.any(), Mockito.anyString());
      Mockito.verify(masterAttributeService).findDetailByAttributeCode(ATTRIBUTE_CODE);
      Mockito.verify(dimensionMappingService)
          .fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode());
    }
  }

  @Test
  public void upsertSizeChartTest4() throws Exception {
      Mockito.when(
          sizeChartService.findBySizeChartCodeAndBusinessPartnerCodeAndMarkForDeleteFalse(STORE_ID, SIZE_CHART_CODE,
              "DR6-44662")).thenReturn(null);
      sizeChartRequest.setName("sizeChartNameEdited");
      sizeChartRequest.setSizeChartCode(SIZE_CHART_CODE);
      Mockito.when(kafkaTopicProperties.getSizeChartUpdateEventName()).thenReturn(SIZE_CHART_UPDATE_EVENT_NAME);
      Mockito.when(brandService.findByBrandCodeCached(STORE_ID, sizeChartRequest.getBrandCode()))
          .thenReturn(brandResponse);
      Mockito.when(sizeChartService.upsertSizeChart(sizeChartRequest, STORE_ID)).thenReturn(sizeChart);
      Mockito.when(masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode()))
          .thenReturn(attribute);
      Mockito.when(sizeChartService.findByNameAndBusinessPartnerCode(STORE_ID, sizeChartRequest.getName(),
          sizeChartRequest.getBusinessPartnerCode())).thenReturn(null);
      Mockito.when(dimensionMappingService.fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode()))
          .thenReturn(List.of(dimensionMapping1, dimensionMapping2));
      sizeChartRequest.setBusinessPartnerCode("DR6-44662");
      try {
        sizeChartWrapperServiceBean.upsertSizeChart(STORE_ID, sizeChartRequest);
      } finally {
        Mockito.verify(masterAttributeService).findDetailByAttributeCode(ATTRIBUTE_CODE);
        Mockito.verify(dimensionMappingService)
            .fetchDimensionMappingForAttribute(STORE_ID, attribute.getAttributeCode());
        Mockito.verify(sizeChartService).upsertSizeChart(Mockito.any(), Mockito.anyString());
        Mockito.verify(sizeChartService).findByNameAndBusinessPartnerCode(STORE_ID, "sizeChartNameEdited",
            sizeChartRequest.getBusinessPartnerCode());
        Mockito.verify(sizeChartService)
            .findBySizeChartCodeAndBusinessPartnerCodeAndMarkForDeleteFalse(STORE_ID, SIZE_CHART_CODE, "DR6-44662");
      }
    }

}
