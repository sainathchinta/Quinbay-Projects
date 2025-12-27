package com.gdn.x.product.service.impl;

import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.dao.api.ProductScoreHistoryL3Repository;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.ProductScoreHistoryL3;

public class ProductScoreHistoryL3ServiceImplTest {

  private static final String STORE_ID = "STORE_ID";
  private static final String PRODUCT_SKU = "productSku";

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private ProductScoreHistoryL3ServiceImpl productScoreHistoryL3Service;

  @Mock
  private ProductScoreHistoryL3Repository productScoreHistoryL3Repository;

  @Captor
  private ArgumentCaptor<ProductScoreHistoryL3> productScoreHistoryL3ArgumentCaptor;

  private ProductScore productScore;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    productScore = new ProductScore();
    productScore.setMandatoryAttributeScore(1.0);
    productScore.setProductTitleScore(1.0);
    productScore.setDescriptionScore(1.0);
    productScore.setUspScore(1.0);
    productScore.setRecommendedAttributeScore(1.0);
    productScore.setRemainingAttributeScore(1.0);
    productScore.setVideoUrlScore(1.0);
    productScore.setImageScore(1.0);
    productScore.setVariantCreatingScore(1.0);
    productScore.setTotalScore(1.0);
  }

  @Test
  public void saveProductScoreHistoryL3Test() throws Exception {
    this.productScoreHistoryL3Service.saveProductScoreHistoryL3(STORE_ID, PRODUCT_SKU, productScore, getProductScore());
    Mockito.verify(this.productScoreHistoryL3Repository).save(productScoreHistoryL3ArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productScoreHistoryL3ArgumentCaptor.getValue().getProductSku());
  }

  @Test
  public void saveProductScoreHistoryL3NullTest() throws Exception {
    this.productScoreHistoryL3Service.saveProductScoreHistoryL3(STORE_ID, PRODUCT_SKU, null, getProductScore());
    Mockito.verify(this.productScoreHistoryL3Repository).save(productScoreHistoryL3ArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productScoreHistoryL3ArgumentCaptor.getValue().getProductSku());
  }

  @Test
  public void saveProductScoreHistoryL3NullTest2() throws Exception {
    this.productScoreHistoryL3Service.saveProductScoreHistoryL3(STORE_ID, PRODUCT_SKU, getProductScore(), null);
    Mockito.verify(this.productScoreHistoryL3Repository).save(productScoreHistoryL3ArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productScoreHistoryL3ArgumentCaptor.getValue().getProductSku());
  }

  @Test
  public void saveProductScoreHistoryL3NullConditionTest() throws Exception {
    this.productScoreHistoryL3Service
        .saveProductScoreHistoryL3(STORE_ID, PRODUCT_SKU, getProductScore(), getProductScore());
  }

  private ProductScore getProductScore() {
    ProductScore productScore = new ProductScore();
    productScore.setMandatoryAttributeScore(2.0);
    productScore.setProductTitleScore(2.0);
    productScore.setDescriptionScore(2.0);
    productScore.setUspScore(2.0);
    productScore.setRecommendedAttributeScore(2.0);
    productScore.setRemainingAttributeScore(2.0);
    productScore.setVideoUrlScore(2.0);
    productScore.setImageScore(2.0);
    productScore.setVariantCreatingScore(2.0);
    productScore.setTotalScore(2.0);
    return productScore;
  }
}
