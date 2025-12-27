package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.exception.ValidationException;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.util.BeanUtils;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.SizeChartFilterRequestDTO;
import com.gdn.x.productcategorybase.dto.request.SizeChartFilterRequest;
import com.gdn.x.productcategorybase.dto.request.SizeChartRequest;
import com.gdn.x.productcategorybase.dto.response.SizeChartDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SizeChartResponse;
import com.gdn.x.productcategorybase.entity.SizeChart;
import com.gdn.x.productcategorybase.repository.SizeChartRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.SizeChartService;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import com.gdn.x.productcategorybase.util.ValidationUtil;

@Service
@Transactional(readOnly = true)
public class SizeChartServiceBean implements SizeChartService {

  @Autowired
  private SizeChartRepository sizeChartRepository;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private AttributeService attributeService;

  @Value("${populate.description}")
  private boolean populateDescription;

  @Value("${sort.external.size.charts.by.created.date.descending}")
  private boolean sortExternalSizeChartsByCreatedDateDescending;

  @Override
  @Transactional
  public SizeChart upsertSizeChart(SizeChartRequest sizeChartRequest, String storeId)
    throws Exception {
    SizeChart entity = new SizeChart();
    if (StringUtils.isBlank(sizeChartRequest.getSizeChartCode())) {
      sizeChartRequest.setSizeChartCode(generateSizeChartCode());
    } else {
      entity = findBySizeChartCodeAndMarkForDeleteFalse(storeId,
        sizeChartRequest.getSizeChartCode());
    }
    ConverterUtil.constructSizeChart(sizeChartRequest, entity, storeId);
    return sizeChartRepository.save(entity);
  }

  @Override
  public SizeChart findBySizeChartCodeAndMarkForDeleteFalse(String storeId, String sizeChartCode) {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(storeId),
      ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    ValidationUtil.checkParameter(StringUtils.isNotBlank(sizeChartCode),
      ErrorMessage.SIZE_CHART_CODE_CANNOT_BE_BLANK.getMessage());
    SizeChart sizeChart =
      sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(storeId, sizeChartCode,
        false);
    ValidationUtil.checkParameter(Objects.nonNull(sizeChart),
        ErrorMessage.SIZE_CHART_NOT_FOUND_WITH_SIZE_CHART_CODE_ERROR_CODE.getMessage(),
        ErrorMessage.SIZE_CHART_NOT_FOUND_WITH_SIZE_CHART_CODE.getMessage());
    return sizeChart;
  }

  @Override
  public List<SizeChart> findSizeChartsBySizeChartCode(String storeId,
      List<String> sizeChartCodes) {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(storeId),
        ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    ValidationUtil.checkParameter(CollectionUtils.isNotEmpty(sizeChartCodes),
        ErrorMessage.SIZE_CHART_CODE_CANNOT_BE_BLANK.getMessage());
    return sizeChartRepository.findByStoreIdAndSizeChartCodeInAndMarkForDeleteFalse(storeId,
        sizeChartCodes);
  }

  @Override
  public String generateSizeChartCode() {
    String sequence =
      Long.toString(sizeChartRepository.getSequenceForSizeChart(Constants.PREFIX_SIZE_CHART_CODE));
    return CommonUtil.generateSizeChartCode(sequence);
  }

  @Override
  public Page<SizeChartResponse> filter(String storeId, SizeChartFilterRequest request,
      Pageable pageable) throws Exception {
    final Map<String, Attribute> attributeCodeDetailMap = new HashMap<>();
    SizeChartFilterRequestDTO sizeChartFilterRequestDTO = new SizeChartFilterRequestDTO();
    BeanUtils.copyProperties(request, sizeChartFilterRequestDTO);
    Page<SizeChart> sizeCharts =
        sizeChartRepository.findByStoreIdAndFilterAppliedAndBusinessPartnerCodeAndMarkForDeleteFalse(
            storeId, sizeChartFilterRequestDTO, pageable,
            sortExternalSizeChartsByCreatedDateDescending);
    if (populateDescription) {
      Set<String> attributeCodeSet =
        sizeCharts.stream().map(SizeChart::getSizeAttributeCode).collect(Collectors.toSet());
      List<Attribute> attributeList =
        attributeService.findDetailByStoreIdAndAttributeCodeList(storeId,
          new ArrayList<>(attributeCodeSet));
      attributeList.forEach(
        attribute -> attributeCodeDetailMap.put(attribute.getAttributeCode(), attribute));
    }
    List<SizeChartResponse> responseList = sizeCharts.get()
      .map(sizeChart -> ConverterUtil.convertToResponse(sizeChart, attributeCodeDetailMap))
      .collect(Collectors.toList());
    return new PageImpl<>(responseList, pageable, sizeCharts.getTotalElements());
  }

  @Override
  public SizeChartDetailResponse fetchSizeChartDetails(String storeId, String sizeChartCode)
    throws Exception {
    SizeChart sizeChartEntity = findBySizeChartCodeAndMarkForDeleteFalse(storeId, sizeChartCode);
    return ConverterUtil.convertToSizeChartDetailResponse(sizeChartEntity);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateSizeChartStatusBySizeChartCode(String storeId, String sizeChartCode,
      String businessPartnerCode, Boolean waitingDeletion, Boolean markForDelete) {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(storeId),
        ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    ValidationUtil.checkParameter(StringUtils.isNotBlank(sizeChartCode),
        ErrorMessage.SIZE_CHART_CODE_CANNOT_BE_BLANK.getMessage());
    ValidationUtil.checkParameter(StringUtils.isNotBlank(businessPartnerCode),
        ErrorMessage.BUSINESS_PARTNER_CODE_CANNOT_BE_BLANK.getMessage());
    SizeChart sizeChartEntity =
        findBySizeChartCodeAndBusinessPartnerCodeAndMarkForDeleteFalse(storeId, sizeChartCode,
            businessPartnerCode);
    if(Objects.nonNull(waitingDeletion)) {
      sizeChartEntity.setWaitingDeletion(waitingDeletion);
    }
    if(BooleanUtils.isTrue(markForDelete)) {
      sizeChartEntity.setMarkForDelete(true);
    }
    sizeChartRepository.save(sizeChartEntity);
  }

  @Override
  public SizeChart findBySizeChartCodeAndBusinessPartnerCodeAndMarkForDeleteFalse(String storeId,
      String sizeChartCode, String businessPartnerCode) {
    SizeChart sizeChart =
        sizeChartRepository.findByStoreIdAndSizeChartCodeAndBusinessPartnerCodeAndMarkForDelete(
            storeId, sizeChartCode, businessPartnerCode, false);
    ValidationUtil.checkParameter(Objects.nonNull(sizeChart),
        ErrorMessage.SIZE_CHART_NOT_FOUND_WITH_SIZE_CHART_CODE_AND_BUSINESS_PARTNER_CODE.getMessage());
    return sizeChart;
  }

  @Override
  public SizeChartResponse findByNameAndBusinessPartnerCode(String storeId, String sizeChartName,
      String businessPartnerCode) {
    SizeChart sizeChart =
        sizeChartRepository.findByStoreIdAndNameAndBusinessPartnerCodeAndMarkForDelete(
            storeId, sizeChartName, businessPartnerCode, false);
    return ConverterUtil.convertToResponse(sizeChart, null);
  }

  @Override
  public boolean validateCategoryAttributesForSizeChart(String storeId, String sizeChartCode, String categoryCode)
      throws Exception {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(categoryCode),
        ErrorMessage.CATEGORY_ID_MUST_NOT_BE_BLANK.getMessage());
    boolean validateIsCategoryCn = this.categoryService.validateIsCategoryCn(storeId, categoryCode);
    if (!validateIsCategoryCn) {
      return false;
    }
    SizeChart sizeChart = this.findBySizeChartCodeAndMarkForDeleteFalse(storeId, sizeChartCode);
    String attributeCode = sizeChart.getSizeAttributeCode();
    return attributeService.getAttributeBasicDetailByCategoryCode(storeId, categoryCode).stream().anyMatch(
        attributeBasicDetailDTO -> attributeCode.equals(attributeBasicDetailDTO.getAttributeCode())
            && attributeBasicDetailDTO.isSizeAttribute());
  }

  @Override
  public void validateSizeChartForBusinessPartnerCodeAndCategoryCode(String storeId,
      String sizeChartCode, String businessPartnerCode, String categoryCode) throws Exception {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(categoryCode),
        ErrorMessage.CATEGORY_CODE_MUST_NOT_BE_BLANK.getMessage());
    SizeChart sizeChart =
        sizeChartRepository.findByStoreIdAndSizeChartCodeAndMarkForDelete(storeId, sizeChartCode,
            false);

    if (Objects.isNull(sizeChart) ||
        (!StringUtils.equals(Constants.INTERNAL, sizeChart.getBusinessPartnerCode()))
            && !StringUtils.equals(sizeChart.getBusinessPartnerCode(), businessPartnerCode)) {
      throw new ValidationException(ErrorMessage.INVALID_SIZE_CHART_CODE_ERROR_CODE.getMessage(),
          ErrorMessage.INVALID_SIZE_CHART_CODE.name());
    }

    String attributeCode = sizeChart.getSizeAttributeCode();
    attributeService.getAttributeBasicDetailByCategoryCode(storeId, categoryCode).stream().filter(
        attributeBasicDetailDTO -> attributeCode.equals(attributeBasicDetailDTO.getAttributeCode())
            && attributeBasicDetailDTO.isSizeAttribute()).findAny().orElseThrow(
        () -> new ValidationException(
            ErrorMessage.SIZE_CHART_IS_NOT_VALID_FOR_THE_CATEGORY_ERROR_CODE.getMessage(),
            ErrorMessage.SIZE_CHART_IS_NOT_VALID_FOR_THE_CATEGORY.name()));
  }
}
