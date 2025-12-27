package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryRestrictedKeyword;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import com.gdn.x.productcategorybase.repository.CategoryRestrictedKeywordRepository;
import com.gdn.x.productcategorybase.service.CategoryRestrictedKeywordService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.RestrictedKeywordService;
import com.gdn.x.productcategorybase.util.ConverterUtil;


import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class CategoryRestrictedKeywordServiceImpl implements CategoryRestrictedKeywordService {

  @Autowired
  private CategoryRestrictedKeywordRepository categoryRestrictedKeywordRepository;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private RestrictedKeywordService restrictedKeywordService;

  @Override
  public List<CategoryRestrictedKeyword> findByStoreIdAndCategoryCode(String storeId, String categoryCode) {
    List<CategoryRestrictedKeyword> categoryRestrictedKeywordList =
        this.categoryRestrictedKeywordRepository.findByStoreIdAndCategoryCode(storeId, categoryCode);
    return setRestrictedKeywordObject(storeId, categoryRestrictedKeywordList);
  }

  private List<CategoryRestrictedKeyword> setRestrictedKeywordObject(String storeId,
      List<CategoryRestrictedKeyword> categoryRestrictedKeywordList) {
    if (CollectionUtils.isEmpty(categoryRestrictedKeywordList)) {
      return categoryRestrictedKeywordList;
    }
    Map<String, RestrictedKeyword> idToRestrictedKeywordMap = this.restrictedKeywordService.findByKeywordId(storeId,
            categoryRestrictedKeywordList.stream().map(CategoryRestrictedKeyword::getRestrictedKeywordId)
                .collect(Collectors.toList())).stream()
        .collect(Collectors.toMap(RestrictedKeyword::getId, Function.identity()));
    categoryRestrictedKeywordList.forEach(categoryRestrictedKeyword -> categoryRestrictedKeyword.setRestrictedKeyword(
        idToRestrictedKeywordMap.getOrDefault(categoryRestrictedKeyword.getRestrictedKeywordId(),
            new RestrictedKeyword())));
    return categoryRestrictedKeywordList;
  }

  @Override
  public void updateCategoryRestrictedKeywordAndEvictCache(String storeId, String categoryCode,
      List<CategoryRestrictedKeyword> categoryRestrictedKeywordList) {
    this.categoryRestrictedKeywordRepository.saveAll(categoryRestrictedKeywordList);
    this.applicationCacheServiceBean.evictCategoryRestrictedKeywordCache(storeId, categoryCode);
  }

  @Override
  public Page<CategoryRestrictedKeyword> findByStoreIdAndCategoryCode(String storeId, String categoryCode,
      Pageable pageable) {
    Page<CategoryRestrictedKeyword> categoryRestrictedKeywordPage = this.categoryRestrictedKeywordRepository
        .findByStoreIdAndCategoryCodeAndMarkForDeleteFalseOrderByUpdatedDateDescIdDesc(storeId, categoryCode, pageable);
    setRestrictedKeywordObject(storeId, categoryRestrictedKeywordPage.getContent());
    return categoryRestrictedKeywordPage;
  }

  @Override
  @Async
  public void addAndDeleteCategoryRestrictedKeywordsForChildCategories(String storeId,
      List<RestrictedKeyword> addedRestrictedKeywords, Category category, List<CategoryKeywordsUpdateDTO> deletedRestrictedKeyword,
      Map<String, CategoryKeywordsUpdateDTO> addedRestrictedKeywordAndRequestMap)
      throws Exception {
    List<Category> allChildCategories = new LinkedList<>(
        this.categoryService.findAllChildForC1CategoryCodesTree(storeId, category.getCategoryCode()));
    allChildCategories.remove(category);
    if (CollectionUtils.isNotEmpty(allChildCategories)) {
      List<String> categoryCodes =
          allChildCategories.stream().map(Category::getCategoryCode).collect(Collectors.toList());
      log.info(
          "adding the restricted keywords mapping {} and deleting the restricted keywords with Id : {} for all CategoryCodes : {}",
          addedRestrictedKeywords, deletedRestrictedKeyword, categoryCodes);
      for (Category childCategory : allChildCategories) {
        try {
          this.addAndDeleteCategoryRestrictedKeywordsMappings(storeId, addedRestrictedKeywords, childCategory,
              deletedRestrictedKeyword, addedRestrictedKeywordAndRequestMap);
        } catch (Exception ex) {
          log.error("Failed to update restricted keyword mapping for categoryCode : {}", category.getCategoryCode(),
              ex);
        }
      }
    }
  }

  @Override
  public CategoryRestrictedKeywordResponse getCategoryRestrictedKeywordById(String storeId, String id) {
    CategoryRestrictedKeyword categoryRestrictedKeyword = this.categoryRestrictedKeywordRepository.findByStoreIdAndId(storeId,id);
    if (Objects.isNull(categoryRestrictedKeyword)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ErrorMessage.CATEGORY_RESTRICTED_KEYWORD_ID_NOT_FOUND + id);
    }
    return ConverterUtil
        .toCategoryRestrictedKeywordResponse(categoryRestrictedKeyword);
  }
  @Override
  public void addAndDeleteCategoryRestrictedKeywordsMappings(String storeId,
      List<RestrictedKeyword> addedRestrictedKeywords, Category category, List<CategoryKeywordsUpdateDTO> deletedRestrictedKeyword,
      Map<String, CategoryKeywordsUpdateDTO> addedRestrictedKeywordAndRequestMap)
      throws Exception {
    List<CategoryRestrictedKeyword> categoryRestrictedKeywordsChanged = new ArrayList<>();
    List<CategoryRestrictedKeyword> existingCategoryRestrictedKeywordList =
        this.findByStoreIdAndCategoryCode(storeId, category.getCategoryCode());
    Map<String, CategoryRestrictedKeyword> existingCategoryRestrictedKeywordMap = new HashMap<>();
    if (Objects.nonNull(existingCategoryRestrictedKeywordList)) {
      for (CategoryRestrictedKeyword categoryRestrictedKeyword : existingCategoryRestrictedKeywordList) {
        if (Objects.nonNull(categoryRestrictedKeyword))
          existingCategoryRestrictedKeywordMap
              .put(categoryRestrictedKeyword.getRestrictedKeywordId(), categoryRestrictedKeyword);
      }
    }

    for (RestrictedKeyword addedRestrictedKeyword : addedRestrictedKeywords) {
      CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO = addedRestrictedKeywordAndRequestMap
          .getOrDefault(addedRestrictedKeyword.getKeyword().toLowerCase(), new CategoryKeywordsUpdateDTO());
      if (!Optional.ofNullable(categoryKeywordsUpdateDTO.getExclusionList()).orElse(new HashSet<>())
          .contains(category.getCategoryCode()) && !Optional.ofNullable(categoryKeywordsUpdateDTO.getExclusionList())
          .orElse(new HashSet<>()).contains(category.getParentCategoryId())) {
        if (existingCategoryRestrictedKeywordMap.containsKey(addedRestrictedKeyword.getId()) && ConverterUtil.isCategoryRestrictedKeywordDataUpdated(categoryKeywordsUpdateDTO,
            existingCategoryRestrictedKeywordMap.get(addedRestrictedKeyword.getId()))) {
          CategoryRestrictedKeyword changedCategoryRestrictedKeyword = existingCategoryRestrictedKeywordMap.get(addedRestrictedKeyword.getId());
          changedCategoryRestrictedKeyword.setMarkForDelete(false);
          changedCategoryRestrictedKeyword.setCategory(category);
          changedCategoryRestrictedKeyword.setType(categoryKeywordsUpdateDTO.getType());
          changedCategoryRestrictedKeyword.setAction(categoryKeywordsUpdateDTO.getAction());
          changedCategoryRestrictedKeyword.setMessage(categoryKeywordsUpdateDTO.getMessage());
          changedCategoryRestrictedKeyword.setDestinationCategory(categoryKeywordsUpdateDTO.getDestinationCategory());
          categoryRestrictedKeywordsChanged.add(changedCategoryRestrictedKeyword);
        } else if (!existingCategoryRestrictedKeywordMap.containsKey(addedRestrictedKeyword.getId())) {
          categoryRestrictedKeywordsChanged.add(
              ConverterUtil.toCategoryRestrictedKeyword(storeId, addedRestrictedKeyword, category,
                  categoryKeywordsUpdateDTO));
        }
      } else {
        categoryKeywordsUpdateDTO.getExclusionList().add(category.getId());
      }
    }

    for (CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO : deletedRestrictedKeyword) {
      if (!Optional.ofNullable(categoryKeywordsUpdateDTO.getExclusionList()).orElse(new HashSet<>())
          .contains(category.getCategoryCode()) && !Optional.ofNullable(categoryKeywordsUpdateDTO.getExclusionList())
          .orElse(new HashSet<>()).contains(category.getParentCategoryId())) {
        String deletedRestrictedKeywordId = categoryKeywordsUpdateDTO.getKeywordId();
        if (existingCategoryRestrictedKeywordMap.containsKey(deletedRestrictedKeywordId)
            && !existingCategoryRestrictedKeywordMap.get(deletedRestrictedKeywordId).isMarkForDelete()) {
          existingCategoryRestrictedKeywordMap.get(deletedRestrictedKeywordId).setMarkForDelete(true);
          categoryRestrictedKeywordsChanged.add(existingCategoryRestrictedKeywordMap.get(deletedRestrictedKeywordId));
        }
      } else {
        categoryKeywordsUpdateDTO.getExclusionList().add(category.getId());
      }
    }
    log.info("Updating the category : {} with category restricted keywords mappings : {}", category.getCategoryCode(),
        categoryRestrictedKeywordsChanged.stream().map(CategoryRestrictedKeyword::getRestrictedKeyword));
    this.updateCategoryRestrictedKeywordAndEvictCache(storeId, category.getCategoryCode(),
        categoryRestrictedKeywordsChanged);
  }

  @Override
  public List<CategoryRestrictedKeywordResponse> getCategoryRestrictedKeywordByCategoryCodeAndKeywordIds(String storeId,
      String categoryCode, List<String> keywordIds) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(categoryCode),
        ErrorMessage.CATEGORY_CODE_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(keywordIds),
        ErrorMessage.KEYWORD_IDS_MUST_NOT_BE_EMPTY.getMessage());
    List<CategoryRestrictedKeyword> categoryRestrictedKeywordList =
        categoryRestrictedKeywordRepository.findByStoreIdAndCategoryCodeAndRestrictedKeywordIdIn(storeId, categoryCode,
            keywordIds);
    setRestrictedKeywordObject(storeId, categoryRestrictedKeywordList);
    List<CategoryRestrictedKeywordResponse> restrictedKeywordResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(categoryRestrictedKeywordList)) {
      restrictedKeywordResponses =
          categoryRestrictedKeywordList.stream().map(ConverterUtil::toCategoryRestrictedKeywordResponseWithKeyword)
              .collect(Collectors.toList());
    }
    return restrictedKeywordResponses;
  }
}
