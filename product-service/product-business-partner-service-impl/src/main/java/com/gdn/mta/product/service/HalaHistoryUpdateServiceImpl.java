package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


import com.gda.mta.product.dto.response.HalalProductHistoryResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.entity.HalalProductHistory;
import com.gdn.mta.product.repository.HalalProductHistoryRepository;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.x.product.domain.event.model.HalalHistoryUpdateEventModel;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class HalaHistoryUpdateServiceImpl implements HalalHistoryUpdateService{

  @Autowired
  HalalProductHistoryRepository halalProductHistoryRepository;

  @Override
  public Page<HalalProductHistoryResponse> getHalalProductHistory(String storeId, String productSku, Pageable pageable)
      throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
        ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productSku), ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    Page<HalalProductHistory> halalProductHistories =
        halalProductHistoryRepository.findByStoreIdAndProductSkuOrderByCreatedDateDesc(storeId,
            productSku, pageable);
    return new PageImpl<>(Optional.of(halalProductHistories.getContent()).orElse(new ArrayList<>()).stream()
        .map(ConverterUtil::toHalalProductHistoryResponse).collect(Collectors.toList()), pageable,
        halalProductHistories.getTotalElements());
  }

  @Override
  @Transactional(readOnly = false)
  public void saveHalalHistoryUpdate(HalalHistoryUpdateEventModel historyEventModel) throws Exception {
    validateHalalHistoryUpdateEventModel(historyEventModel);
    HalalProductHistory halalProductHistory = new HalalProductHistory();
    BeanUtils.copyProperties(historyEventModel, halalProductHistory);
    halalProductHistoryRepository.save(halalProductHistory);
  }

  private static void validateHalalHistoryUpdateEventModel(HalalHistoryUpdateEventModel historyEventModel) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(historyEventModel.getProductSku()),
        ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(historyEventModel.getUserName()),
        ErrorMessages.USER_NAME_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(historyEventModel.getActivity()),
        ErrorMessages.ACTIVITY_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(historyEventModel.getPreviousValue()),
        ErrorMessages.PREVIOUS_VALUE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(historyEventModel.getCurrentValue()),
        ErrorMessages.CURRENT_VALUE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(historyEventModel.getStoreId()),
        ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
  }
}
