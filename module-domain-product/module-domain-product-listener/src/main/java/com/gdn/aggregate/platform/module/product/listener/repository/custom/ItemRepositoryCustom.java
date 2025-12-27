package com.gdn.aggregate.platform.module.product.listener.repository.custom;


import java.util.Set;

public interface ItemRepositoryCustom {
    Set<String> findItemCodesAndDeleteByProductSku(String productSku);
}