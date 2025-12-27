package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.CategoryChangeEventType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.OscInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.response.OriginalSalesCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.OscSummaryResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;
import com.gdn.x.productcategorybase.repository.OriginalSalesCategoryRepository;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.OriginalSalesCategoryService;
import com.gdn.x.productcategorybase.util.ConverterUtil;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class OriginalSalesCategoryServiceBean implements OriginalSalesCategoryService {

  @Autowired
  private OriginalSalesCategoryRepository originalSalesCategoryRepository;

  @Autowired
  @Lazy
  private CategoryService categoryService;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public String save(OriginalSalesCategory entity) throws Exception {
    OriginalSalesCategory originalSalesCategory = originalSalesCategoryRepository.saveAndFlush(entity);
    return originalSalesCategory.getId();
  }

  @Override
  public List<OscSummaryResponse> filterSummaryOSC(String oscCode, String keyword, Boolean activated) {
    List<OriginalSalesCategory> originalSalesCategories =
        originalSalesCategoryRepository.findByOscCodeAndNameAndActivated(oscCode, keyword, activated);
    List<OscSummaryResponse> oscSummaryResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(originalSalesCategories)) {
      for (OriginalSalesCategory originalSalesCategory : originalSalesCategories) {
        OscSummaryResponse summaryResponse = new OscSummaryResponse();
        BeanUtils.copyProperties(originalSalesCategory, summaryResponse);
        summaryResponse.setId(originalSalesCategory.getId());
        oscSummaryResponses.add(summaryResponse);
      }
    }
    return oscSummaryResponses;
  }

  @Override
  public OriginalSalesCategory findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(String storeId, String id) {
    GdnPreconditions
        .checkArgument(StringUtils.isNotBlank(storeId), ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(id), ErrorMessage.OSC_ID_MUST_NOT_BE_BLANK.getMessage());
    return originalSalesCategoryRepository.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(storeId, id);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateOsc(String storeId, String userName, OscInfoUpdateDTO oscInfoUpdateDTO) throws Exception {
    List<Category> categoryList = new ArrayList<>();
    OriginalSalesCategory originalSalesCategory =
        originalSalesCategoryRepository.findByOscCode(oscInfoUpdateDTO.getOscCode());
    BeanUtils.copyProperties(oscInfoUpdateDTO, originalSalesCategory, "oscCode");
    if (!oscInfoUpdateDTO.isActivated() && StringUtils.isNotEmpty(oscInfoUpdateDTO.getNewOscCode())) {
      OriginalSalesCategory newOsc = originalSalesCategoryRepository.findByOscCode(oscInfoUpdateDTO.getNewOscCode());
      if (newOsc.isActivated()) {
        categoryList
            .addAll(ConverterUtil.toUpdatedCategory(originalSalesCategory.getMasterCategories(), newOsc, userName));
        originalSalesCategory.setMasterCategories(new ArrayList<>());
        newOsc.setMasterCategories(categoryList);
        ServiceBeanHelper.updateEntity(newOsc, this.originalSalesCategoryRepository);
      } else {
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessage.OSC_IS_INACTIVE.getMessage());
      }
    }
    ServiceBeanHelper.updateEntity(originalSalesCategory, this.originalSalesCategoryRepository);
    for (Category category : categoryList) {
      categoryService.saveUpdatedCategory(category, null, null,
          Collections.singletonList(CategoryChangeEventType.CATEGORY_OSC_MAPPING_CHANGE));
    }
  }

  @Override
  public OriginalSalesCategoryResponse findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id) {
    GdnPreconditions
        .checkArgument(StringUtils.isNotBlank(storeId), ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(id), ErrorMessage.OSC_ID_MUST_NOT_BE_BLANK.getMessage());
    log.info("Fetching original sales category by store ID {} and ID : {}", storeId, id);
    OriginalSalesCategory originalSalesCategory =
        originalSalesCategoryRepository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, id);
    if (Objects.isNull(originalSalesCategory)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Cannot find original sales category with ID : " + id);
    }
    Hibernate.initialize(originalSalesCategory.getMasterCategories());
    return ConverterUtil.toOriginalSalesCategoryResponse(originalSalesCategory);
  }
}
