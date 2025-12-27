package com.gdn.x.productcategorybase.service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.dto.response.CategoryHistoryResponse;
import com.gdn.x.productcategorybase.entity.CategoryHistory;
import com.gdn.x.productcategorybase.repository.CategoryHistoryRepository;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Service
public class CategoryHistoryServiceBean implements CategoryHistoryService {

  @Autowired
  private CategoryHistoryRepository categoryHistoryRepository;

  @Override
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public CategoryHistory saveCategoryHistory(CategoryHistoryEventModel categoryHistoryEventModel) {
    CategoryHistory categoryHistory = new CategoryHistory();
    BeanUtils.copyProperties(categoryHistoryEventModel, categoryHistory);
    categoryHistory.setUpdatedBy(categoryHistoryEventModel.getUserName());
    categoryHistory.setCreatedBy(categoryHistoryEventModel.getUserName());
    categoryHistory.setStoreId(categoryHistoryEventModel.getStoreId());
    log.info("Saving category history {}", categoryHistory);
    return categoryHistoryRepository.save(categoryHistory);
  }

  @Override
  public Page<CategoryHistoryResponse> fetchCategoryHistory(String storeId, String categoryCode,
      int page, int size) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(categoryCode),
        ErrorMessage.CATEGORY_CODE_MUST_NOT_BE_BLANK.getMessage());
    log.info("Fetching category history with category Code {}", categoryCode);
    Page<CategoryHistory> categoryHistory =
        categoryHistoryRepository.findByStoreIdAndCategoryCodeOrderByCreatedDateDesc(storeId,
            categoryCode, PageRequest.of(page, size));
    List<CategoryHistoryResponse> responses =
        categoryHistory.stream().map(ConverterUtil::mapCategoryHistoryToCategoryHistoryResponse)
            .collect(Collectors.toList());
    return new PageImpl<>(responses, PageRequest.of(page, size), categoryHistory.getTotalElements());
  }

}
