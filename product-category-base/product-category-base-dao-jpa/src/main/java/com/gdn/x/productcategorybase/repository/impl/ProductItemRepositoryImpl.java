package com.gdn.x.productcategorybase.repository.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcOperations;

import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.repository.ProductItemRepositoryCustom;

public class ProductItemRepositoryImpl implements ProductItemRepositoryCustom {

  private static final String FIND_BY_UPC_CODES_MATCHES_PAGEABLE_QUERY =
      "SELECT * FROM pcc_product_item WHERE upc_code ~ :upcCodeRegex AND store_id = :storeId ORDER BY upc_code LIMIT :size OFFSET (:size*:page)";

  private static final String FIND_BY_UPC_CODES_MATCHES_EXCLUDE_ONE_ITEM_PAGEABLE_QUERY =
      "SELECT * FROM pcc_product_item WHERE upc_code ~ :upcCodeRegex AND sku_code != :skuCode AND store_id = :storeId ORDER BY upc_code LIMIT :size OFFSET (:size*:page)";

  @Autowired
  private NamedParameterJdbcOperations namedJdbcTemplate;

  @Override
  public Page<ProductItem> findByStoreIdAndUpcCodeExcludeOneItemMatches(String storeId, String upcCodeRegex,
      String skuCode, Pageable pageable) {
    Map<String, Object> parameters = new HashMap<>();
    parameters.put("upcCodeRegex", upcCodeRegex);
    parameters.put("skuCode", skuCode);
    parameters.put("storeId", storeId);
    parameters.put("size", pageable.getPageSize());
    parameters.put("page", pageable.getPageNumber());

    List<ProductItem> productItems = this.namedJdbcTemplate.query(
        ProductItemRepositoryImpl.FIND_BY_UPC_CODES_MATCHES_EXCLUDE_ONE_ITEM_PAGEABLE_QUERY, parameters,
        new BeanPropertyRowMapper(ProductItem.class));

    return new PageImpl<>(productItems, pageable, productItems.size());
  }

  @Override
  public Page<ProductItem> findByStoreIdAndUpcCodeMatches(String storeId, String upcCodeRegex, Pageable pageable) {
    Map<String, Object> parameters = new HashMap<>();
    parameters.put("upcCodeRegex", upcCodeRegex);
    parameters.put("storeId", storeId);
    parameters.put("size", pageable.getPageSize());
    parameters.put("page", pageable.getPageNumber());

    List<ProductItem> productItems =
        this.namedJdbcTemplate.query(ProductItemRepositoryImpl.FIND_BY_UPC_CODES_MATCHES_PAGEABLE_QUERY, parameters,
            new BeanPropertyRowMapper(ProductItem.class));

    return new PageImpl<>(productItems, pageable, productItems.size());
  }
}
