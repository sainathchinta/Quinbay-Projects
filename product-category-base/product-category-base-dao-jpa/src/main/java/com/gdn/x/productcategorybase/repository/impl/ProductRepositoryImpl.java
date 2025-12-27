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

import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.repository.ProductRepositoryCustom;

public class ProductRepositoryImpl implements ProductRepositoryCustom {

  private static final String FIND_BY_STORE_ID_AND_CATEGORIES_CODE =
      "SELECT p.* FROM pcc_product p " + "INNER JOIN pcc_product_category pc ON p.id = pc.product_id "
          + "INNER JOIN pcc_category c ON pc.category_id = c.id " + "AND p.store_id = :storeId "
          + "AND p.mark_for_delete = false " + "AND category_code IN (:categoriesCode) " + "ORDER BY p.name "
          + "LIMIT :size OFFSET (:size*:page)";

  @Autowired
  private NamedParameterJdbcOperations namedJdbcTemplate;

  @Override
  public Page<Product> findByStoreIdAndCategoriesCode(String storeId, List<String> categoriesCode, Pageable pageable) {
    Map<String, Object> parameters = new HashMap<>();
    parameters.put("storeId", storeId);
    parameters.put("categoriesCode", categoriesCode);
    parameters.put("size", pageable.getPageSize());
    parameters.put("page", pageable.getPageNumber());

    List<Product> products = this.namedJdbcTemplate.query(ProductRepositoryImpl.FIND_BY_STORE_ID_AND_CATEGORIES_CODE,
        parameters, new BeanPropertyRowMapper(Product.class));

    return new PageImpl<Product>(products, pageable, products.size());
  }
}
