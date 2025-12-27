package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.Mockito.never;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import com.gdn.x.productcategorybase.repository.ProductScoreUpdateRepository;

public class ProductScoreTest {

  @Mock
  private ProductScoreUpdateRepository productScoreUpdateRepository;

  @InjectMocks
  private ProductScoreBean service;

  private String PRODUCT_CODE1 = "productCode1";
  private String PRODUCT_CODE2 = "productCode2";
  private List<String> productScoreUpdateList;

  @BeforeEach
  public void initialize() throws Exception {
    MockitoAnnotations.initMocks(this);
    productScoreUpdateList = new ArrayList<>();
    productScoreUpdateList.add(PRODUCT_CODE1);
    productScoreUpdateList.add(PRODUCT_CODE2);
  }

  @Test
  public void findByMarkForDeleteFalseAndUpdatedFalseTest() throws Exception {
    when(productScoreUpdateRepository.findByMarkForDeleteFalseAndUpdatedFalse(Mockito.eq(PageRequest.of(0, 10))))
        .thenReturn(new PageImpl<>(productScoreUpdateList, PageRequest.of(0, 10), 2));
    Page<String> page = service.findByMarkForDeleteFalseAndUpdatedFalse(PageRequest.of(0, 10));
    Mockito.verify(productScoreUpdateRepository)
        .findByMarkForDeleteFalseAndUpdatedFalse(Mockito.eq(PageRequest.of(0, 10)));
    Assertions.assertNotNull(page);
    Assertions.assertEquals(2, page.getContent().size());
  }

  @Test
  public void updateProductsTest() {
    Mockito.doNothing().when(productScoreUpdateRepository).updateProducts(productScoreUpdateList);
    service.updateProducts(productScoreUpdateList);
    Mockito.verify(this.productScoreUpdateRepository).updateProducts(productScoreUpdateList);
  }

  @Test
  public void updateProductsEmptyListCheckTest()  {
    service.updateProducts(new ArrayList<>());
    Mockito.verify(this.productScoreUpdateRepository, never()).updateProducts(Mockito.anyList());
  }
 }