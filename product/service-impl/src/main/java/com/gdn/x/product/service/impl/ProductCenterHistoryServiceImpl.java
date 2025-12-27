package com.gdn.x.product.service.impl;

import com.gdn.x.product.dao.api.ProductCenterHistoryRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductCenterActivity;
import com.gdn.x.product.model.entity.ProductCenterHistory;
import com.gdn.x.product.rest.web.model.dto.ProductCenterHistoryResponse;
import com.gdn.x.product.service.api.ProductCenterHistoryService;
import com.gdn.x.product.service.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Date;
import java.util.Optional;
import java.util.stream.Collectors;

@Slf4j
@Service
public class ProductCenterHistoryServiceImpl implements ProductCenterHistoryService {

  private static final String COPY_HISTORY = "Copied to %s";
  private static final String MOVE_HISTORY = "Current category : %s /n New category : %s";
  private static final String ETD_NOTES_UPDATE = "Current ETD Notes value : %s /n New value : %s";
  private static final String DELETE_HISTORY = "Deleted from %s";

  @Autowired private ProductCenterHistoryRepository productCenterHistoryRepository;

  @Async
  @Override
  public void saveProductCenterHistory(String productSku, ProductCenterActivity activity, String requestId,
      String oldValue, String newValue) {
    ProductCenterHistory productCenterHistory = new ProductCenterHistory();
    productCenterHistory.setStoreId(Constants.DEFAULT_STORE_ID);
    productCenterHistory.setCreatedBy(requestId);
    productCenterHistory.setCreatedDate(new Date());
    productCenterHistory.setUpdatedBy(requestId);
    productCenterHistory.setUpdatedDate(new Date());

    productCenterHistory.setActivity(activity.getDescription());
    productCenterHistory.setProductSku(productSku);
    switch (activity) {
      case COPY:
        productCenterHistory.setDescription(String.format(COPY_HISTORY, newValue));
        break;
      case MOVE:
        productCenterHistory
            .setDescription(String.format(MOVE_HISTORY, oldValue, newValue));
        break;
      case DELETE:
        productCenterHistory.setDescription(String.format(DELETE_HISTORY, oldValue));
        break;
      case ETD_NOTES_UPDATE:
        productCenterHistory.setDescription(String.format(ETD_NOTES_UPDATE, oldValue, newValue));
        break;
    }
    productCenterHistoryRepository.save(productCenterHistory);
  }

  @Override
  public Page<ProductCenterHistoryResponse> getProductCenterHistoryByStoreIdAndProductSku(String storeId, String productSku,
      int page, int size) {
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductCenterHistory> productCenterHistoryPage = this.productCenterHistoryRepository
        .findProductCenterHistoryByStoreIdAndProductSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(storeId, productSku, pageable);
    return new PageImpl<>(Optional.ofNullable(productCenterHistoryPage.getContent()).orElse(new ArrayList<>()).stream()
        .map(CommonUtil::toProductCenterHistoryResponse)
        .collect(Collectors.toList()), pageable, productCenterHistoryPage.getTotalElements());
  }
}
