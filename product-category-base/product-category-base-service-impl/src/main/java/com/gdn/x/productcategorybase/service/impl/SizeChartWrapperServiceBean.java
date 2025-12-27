package com.gdn.x.productcategorybase.service.impl;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.model.SizeChartUpdateEventModel;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.SizeChartRequest;
import com.gdn.x.productcategorybase.dto.response.SizeChartResponse;
import com.gdn.x.productcategorybase.dto.response.SizeChartDetailResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.DimensionMapping;
import com.gdn.x.productcategorybase.entity.SizeChart;
import com.gdn.x.productcategorybase.service.DimensionMappingService;
import com.gdn.x.productcategorybase.service.MasterAttributeService;
import com.gdn.x.productcategorybase.service.SizeChartService;
import com.gdn.x.productcategorybase.service.SizeChartServiceWrapper;
import com.gdn.x.productcategorybase.service.brand.BrandService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.gdn.x.productcategorybase.util.ValidationUtil;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import lombok.extern.slf4j.Slf4j;


@Service
@Slf4j
public class SizeChartWrapperServiceBean implements SizeChartServiceWrapper {

  @Autowired
  private SizeChartService sizeChartService;

  @Autowired
  private BrandService brandService;

  @Autowired
  private MasterAttributeService masterAttributeService;

  @Autowired
  private DimensionMappingService dimensionMappingService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  KafkaTopicProperties kafkaTopicProperties;

  @Value("${size.chart.valid.units}")
  private String sizeChartValidUnits;

  @Override
  public void upsertSizeChart(String storeId, SizeChartRequest sizeChartRequest) throws Exception {
    SizeChartResponse sizeChartResponse =
        sizeChartService.findByNameAndBusinessPartnerCode(storeId, sizeChartRequest.getName(),
            sizeChartRequest.getBusinessPartnerCode());
    if (ValidationUtil.isExternal(sizeChartRequest.getBusinessPartnerCode()) && StringUtils.isNotBlank(
        sizeChartRequest.getSizeChartCode())) {
      validateSizeChartUpsertRequestByFetchingSizeChart(storeId, sizeChartRequest);
    } else {
      ValidationUtil.checkParameter(Objects.isNull(sizeChartResponse),
          ErrorMessage.SIZE_CHART_NAME_ALREADY_EXISTS_ERROR_CODE.getMessage(),
          ErrorMessage.SIZE_CHART_NAME_ALREADY_EXISTS.getMessage());
    }
    if(StringUtils.equals(sizeChartRequest.getBusinessPartnerCode(), Constants.INTERNAL)) {
      BrandResponse brandResponse =
          brandService.findByBrandCodeCached(storeId, sizeChartRequest.getBrandCode());
      ValidationUtil.checkParameter(Objects.nonNull(brandResponse),
          ErrorMessage.BRAND_NOT_FOUND_ERROR_CODE.getMessage(),
          ErrorMessage.BRAND_NOT_FOUND.getMessage());
      sizeChartRequest.setBrand(brandResponse.getBrandName());
    }
    Attribute attribute =
      masterAttributeService.findDetailByAttributeCode(sizeChartRequest.getSizeAttributeCode());
    ValidationUtil.checkParameter(attribute.isSizeAttribute(),
        ErrorMessage.ATTRIBUTE_IS_NOT_SIZE_ATTRIBUTE_ERROR_CODE.getMessage(),
        ErrorMessage.ATTRIBUTE_IS_NOT_SIZE_ATTRIBUTE.getMessage());
    sizeChartRequest.setSizeAttributeName(attribute.getName());
    ValidationUtil.validateSelectedValueTypes(attribute, sizeChartRequest.getSelectedValueTypes());
    List<DimensionMapping> dimensionMappingList =
      dimensionMappingService.fetchDimensionMappingForAttribute(storeId,
        attribute.getAttributeCode());
    Map<String, String> dimensionNameToCodeMap =
      ValidationUtil.validateSelectedDimensions(dimensionMappingList,
        sizeChartRequest.getSelectedDimensionCodes());
    ValidationUtil.validateSizeChartRequest(sizeChartRequest, dimensionNameToCodeMap,
        new HashSet<>(Arrays.asList(sizeChartValidUnits.split(Constants.COMMA))));
    SizeChart updatedSizeChart = sizeChartService.upsertSizeChart(sizeChartRequest, storeId);
    SizeChartUpdateEventModel sizeChartUpdateEventMode = CommonUtil.getSizeChartUpdateEventModel(updatedSizeChart);
    log.info("Publishing Size chart Update event {} ", sizeChartUpdateEventMode);
    kafkaPublisher.send(kafkaTopicProperties.getSizeChartUpdateEventName(), updatedSizeChart.getSizeChartCode(),
        sizeChartUpdateEventMode);
  }

  private void validateSizeChartUpsertRequestByFetchingSizeChart(String storeId, SizeChartRequest sizeChartRequest) {
    SizeChart sizeChartResponseWithSizeChartCode =
        sizeChartService.findBySizeChartCodeAndBusinessPartnerCodeAndMarkForDeleteFalse(storeId,
            sizeChartRequest.getSizeChartCode(), sizeChartRequest.getBusinessPartnerCode());
    if (Objects.nonNull(sizeChartResponseWithSizeChartCode)) {
      ValidationUtil.checkParameter(
          StringUtils.equals(sizeChartRequest.getName(), sizeChartResponseWithSizeChartCode.getName()),
          ErrorMessage.SIZE_CHART_NAME_EDIT_NOT_ALLOWED_ERROR_CODE.getMessage(),
          ErrorMessage.SIZE_CHART_NAME_EDIT_NOT_ALLOWED_ERROR_MESSAGE.getMessage());
    }
  }

  @Override
  public SizeChartDetailResponse fetchSizeChartDetails(String storeId, boolean preview,
    String sizeChartCode) throws Exception {

    SizeChartDetailResponse sizeChartDetailResponse =
      sizeChartService.fetchSizeChartDetails(storeId, sizeChartCode);
    if (preview) {
      Attribute attribute = masterAttributeService.findDetailByAttributeCode(
        sizeChartDetailResponse.getSizeAttributeCode());
      sizeChartDetailResponse.setAttributeImageUrl(attribute.getAttributeImageUrl());
      sizeChartDetailResponse.setDescriptionEnglish(attribute.getDescriptionEnglish());
      sizeChartDetailResponse.setDescription(attribute.getDescription());
    }
    return sizeChartDetailResponse;
  }
}
