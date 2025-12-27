package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordActivityHistory;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordHistoryResponse;
import com.gdn.x.productcategorybase.entity.RestrictedKeywordHistory;
import com.gdn.x.productcategorybase.repository.RestrictedKeywordHistoryRepository;
import com.gdn.x.productcategorybase.service.RestrictedKeywordHistoryService;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class RestrictedKeywordHistoryServiceImpl implements RestrictedKeywordHistoryService {

  @Autowired
  private RestrictedKeywordHistoryRepository restrictedKeywordHistoryRepository;

  @Override
  public Page<RestrictedKeywordHistoryResponse> getRestrictedKeywordHistory(String storeId, String keywordId,
      Pageable pageable) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
        ErrorMessage.STORE_ID_MUST_NOT_BE_EMPTY.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(keywordId),
        ErrorMessage.RESTRICTED_KEYWORD_ID_MUST_NOT_BE_EMPTY.getMessage());
    Page<RestrictedKeywordHistory> restrictedKeywordHistories =
        restrictedKeywordHistoryRepository.findByStoreIdAndKeywordIdAndMarkForDeleteFalseOrderByCreatedDateDesc(storeId,
            keywordId, pageable);
    return new PageImpl<>(
        Optional.ofNullable(restrictedKeywordHistories.getContent()).orElse(new ArrayList<>()).stream()
            .map(ConverterUtil::toRestrictedKeywordHistoryResponse).collect(Collectors.toList()), pageable,
        restrictedKeywordHistories.getTotalElements());
  }

  @Override
  @Transactional(readOnly = false)
  public void saveRestrictedKeywordHistory(RestrictedKeywordHistoryEventModel historyEventModel) throws Exception {
    validateRestrictedKeywordHistoryEventModel(historyEventModel);
    List<RestrictedKeywordHistory> restrictedKeywordHistoryList = new ArrayList<>();
    for (RestrictedKeywordActivityHistory restrictedKeywordActivityHistory : historyEventModel.getRestrictedKeywordActivityHistoryList()) {
      RestrictedKeywordHistory restrictedKeywordHistory = new RestrictedKeywordHistory();
      BeanUtils.copyProperties(historyEventModel, restrictedKeywordHistory, "userName",
          "restrictedKeywordActivityHistoryList");
      BeanUtils.copyProperties(restrictedKeywordActivityHistory, restrictedKeywordHistory);
      restrictedKeywordHistoryList.add(restrictedKeywordHistory);
    }
    restrictedKeywordHistoryRepository.saveAll(restrictedKeywordHistoryList);
  }

  private static void validateRestrictedKeywordHistoryEventModel(RestrictedKeywordHistoryEventModel historyEventModel) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(historyEventModel.getStoreId()),
        ErrorMessage.STORE_ID_MUST_NOT_BE_EMPTY.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(historyEventModel.getUserName()),
        ErrorMessage.USER_NAME_MUST_NOT_BE_EMPTY.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(historyEventModel.getKeywordId()),
        ErrorMessage.RESTRICTED_KEYWORD_ID_MUST_NOT_BE_EMPTY.getMessage());
    for (RestrictedKeywordActivityHistory restrictedKeywordActivityHistory : historyEventModel.getRestrictedKeywordActivityHistoryList()) {
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(restrictedKeywordActivityHistory.getActivity()),
          ErrorMessage.ACTIVITY_MUST_NOT_BE_EMPTY.getMessage());
    }
  }
}
