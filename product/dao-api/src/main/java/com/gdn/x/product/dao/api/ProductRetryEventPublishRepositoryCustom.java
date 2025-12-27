package com.gdn.x.product.dao.api;

import java.util.List;

import com.gdn.x.product.enums.RetryPublishStatus;
import com.gdn.x.product.model.entity.ProductRetryEventPublish;

public interface ProductRetryEventPublishRepositoryCustom {

  List<ProductRetryEventPublish> findByStateForRetryPublish(RetryPublishStatus state, int limit);
}
