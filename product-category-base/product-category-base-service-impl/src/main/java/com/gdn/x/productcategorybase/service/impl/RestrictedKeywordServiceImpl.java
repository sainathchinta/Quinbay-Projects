package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateListDTO;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsListingResponse;
import com.gdn.x.productcategorybase.dto.response.UiValidationRestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import com.gdn.x.productcategorybase.repository.RestrictedKeywordRepository;
import com.gdn.x.productcategorybase.service.RestrictedKeywordService;
import com.gdn.x.productcategorybase.util.ConverterUtil;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class RestrictedKeywordServiceImpl implements RestrictedKeywordService {

  @Autowired
  private RestrictedKeywordRepository restrictedKeywordRepository;

  @Override
  @Transactional(readOnly = false)
  public List<RestrictedKeyword> saveRestrictedKeywords(List<RestrictedKeyword> restrictedKeywordList) {
    return this.restrictedKeywordRepository.saveAll(restrictedKeywordList);
  }

  @Override
  public List<RestrictedKeyword> findByKeywordId(String storeId, List<String> keywordIdList) {
    GdnPreconditions
        .checkArgument(CollectionUtils.isNotEmpty(keywordIdList), ErrorMessage.RESTRICTED_KEYWORD_ERROR.getMessage());
    return this.restrictedKeywordRepository.findByStoreIdAndIdInAndMarkForDeleteFalse(storeId, keywordIdList);
  }

  @Override
  public RestrictedKeyword findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(String storeId,
      String keyword) {
    return this.restrictedKeywordRepository
        .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(storeId, keyword);
  }

  @Override
  public void getDeletedRestrictedKeywordId(CategoryKeywordsUpdateListDTO categoryKeywordsUpdateListDTO) {
    Set<CategoryKeywordsUpdateDTO> keywordsRequestsWithoutKeywordIds =
        categoryKeywordsUpdateListDTO.getDeletedRestrictedKeywords().stream()
            .filter(categoryKeywordsUpdateDTO -> StringUtils.isBlank(categoryKeywordsUpdateDTO.getKeywordId()))
            .collect(Collectors.toSet());
    if (CollectionUtils.isNotEmpty(keywordsRequestsWithoutKeywordIds)) {
      Set<String> keywordsWithoutKeywordIds =
          keywordsRequestsWithoutKeywordIds.stream().map(CategoryKeywordsUpdateDTO::getKeyword)
              .collect(Collectors.toSet());
      List<RestrictedKeyword> restrictedKeywordList =
          restrictedKeywordRepository.findByStoreIdAndKeywordInAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
              keywordsWithoutKeywordIds);
      if (CollectionUtils.isNotEmpty(restrictedKeywordList)) {
        Map<String, String> keywordAndKeywordIdMap = restrictedKeywordList.stream()
            .collect(Collectors.toMap(RestrictedKeyword::getKeyword, RestrictedKeyword::getId));
        keywordsRequestsWithoutKeywordIds.forEach(categoryKeywordsUpdateDTO -> categoryKeywordsUpdateDTO.setKeywordId(
            keywordAndKeywordIdMap.get(categoryKeywordsUpdateDTO.getKeyword())));
      }
      categoryKeywordsUpdateListDTO.getDeletedRestrictedKeywords()
          .removeIf(categoryKeywordsUpdateDTO -> StringUtils.isBlank(categoryKeywordsUpdateDTO.getKeywordId()));
    }
  }

  @Override
  public Page<RestrictedKeyword> getRestrictedKeywordSuggestions(String storeId, String keyword, Pageable pageable) {
    return this.restrictedKeywordRepository
        .findByStoreIdAndKeywordContainingIgnoreCaseAndMarkForDeleteFalse(storeId, keyword, pageable);
  }

  @Override
  public Page<RestrictedKeywordsListingResponse> getRestrictedKeywordForListing(String storeId, String keyword,
      Pageable pageable) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
        ErrorMessage.STORE_ID_MUST_NOT_BE_EMPTY.getMessage());
    Page<RestrictedKeyword> restrictedKeywords = null;
    if (StringUtils.isNotEmpty(keyword)) {
      restrictedKeywords =
          this.restrictedKeywordRepository.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalseOrderByUpdatedDateDesc(
              storeId, keyword, pageable);
    } else {
      restrictedKeywords =
          this.restrictedKeywordRepository.findByStoreIdAndMarkForDeleteFalseAndValidateOnUiNotNullAndValidateByDsNotNullOrderByUpdatedDateDesc(
              storeId, pageable);
    }
    return getRestrictedKeywordsListingResponses(pageable, restrictedKeywords);
  }

  private static PageImpl<RestrictedKeywordsListingResponse> getRestrictedKeywordsListingResponses(Pageable pageable,
      Page<RestrictedKeyword> restrictedKeywords) {
    return new PageImpl<>(Optional.ofNullable(restrictedKeywords.getContent()).orElse(new ArrayList<>()).stream()
        .map(ConverterUtil::toRestrictedKeywordsListingResponse).collect(Collectors.toList()), pageable,
        restrictedKeywords.getTotalElements());
  }

  @Override
  public List<UiValidationRestrictedKeywordsResponse> getListOfRestrictedKeywordsForUiValidation(String storeId) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
        ErrorMessage.STORE_ID_MUST_NOT_BE_EMPTY.getMessage());
    List<RestrictedKeyword> restrictedKeywords =
        this.restrictedKeywordRepository.findByStoreIdAndMarkForDeleteFalseAndValidateOnUi(storeId, true);
    return ConverterUtil.toListOfRestrictedKeywordsForUiValidation(restrictedKeywords);
  }
}
