package com.gdn.x.productcategorybase.util;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateDTO;
import com.gdn.x.productcategorybase.entity.Product;

public class ObjectCopyUtilTest {

  private static SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO;
  private static Product product;
  private static final String BRAND = "brand";
  private static final String PRODUCT_CODE = "productCode";
  private static final String ORIGINAL_PRODUCT_CODE = "originalProductCode";
  private static final Double LENGTH = 100D;
  private static final Integer DANGEROUS_GOOD_LEVEL = 1;

  @BeforeEach
  public void setUp(){
    simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    simpleMasterProductUpdateDTO.setBrand(BRAND);
    simpleMasterProductUpdateDTO.setProductCode(PRODUCT_CODE);
    simpleMasterProductUpdateDTO.setDangerousGoodsLevel(DANGEROUS_GOOD_LEVEL);
    simpleMasterProductUpdateDTO.setLength(LENGTH);

    product = new Product();
    product.setProductCode(ORIGINAL_PRODUCT_CODE);
  }

  @Test
  public void copyPropertiesIgnoreNullValues() {
    ObjectCopyUtil.copyPropertiesIgnoreNullValues(simpleMasterProductUpdateDTO, product, PRODUCT_CODE);
    assertEquals(BRAND, product.getBrand());
    assertEquals(LENGTH, product.getLength());
    assertEquals(ORIGINAL_PRODUCT_CODE, product.getProductCode());
  }

}