package com.gdn.x.productcategorybase.helper;

import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import com.gdn.x.productcategorybase.service.RestrictedKeywordService;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class RestrictedKeywordServiceHelper {

  @Autowired
  private RestrictedKeywordService restrictedKeywordService;

  public Page<RestrictedKeywordsResponse> getRestrictedKeywordsResponses(String storeId,
    String keyword, Pageable pageable) {
    if (StringUtils.isNotEmpty(keyword)) {
      Page<RestrictedKeyword> restrictedKeywordsPage =
        restrictedKeywordService.getRestrictedKeywordSuggestions(storeId, keyword, pageable);
      List<RestrictedKeywordsResponse> restrictedKeywordsResponseList =
        restrictedKeywordsPage.getContent().stream()
          .sorted(Comparator.comparingInt(keyword2 -> keyword2.getKeyword().length()))
          .map(ConverterUtil::toRestrictedKeywordsResponseWithValidationFlagFromRestrictedKeyword)
          .collect(Collectors.toList());
      return new PageImpl<>(restrictedKeywordsResponseList, pageable,
        restrictedKeywordsResponseList.size());
    }
    return new PageImpl<>(Collections.emptyList());
  }
}
