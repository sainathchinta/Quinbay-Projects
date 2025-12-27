package com.gdn.x.product.service.impl;

import static org.mockito.MockitoAnnotations.openMocks;

import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.x.product.dao.api.ProductCenterHistoryRepository;
import com.gdn.x.product.enums.ProductCenterActivity;
import com.gdn.x.product.model.entity.ProductCenterHistory;
import com.gdn.x.product.rest.web.model.dto.ProductCenterHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;

public class ProductCenterHistoryServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "request_id";
  private static final String PRODUCT_SKU = "productSku";
  private static final String NEW_CATEGORY_NAME = "newCategoryName";
  private static final String OLD_CATEGORY_NAME = "oldCategoryName";
  private static final int page = 0;
  private static final int size = 10;

  private CategoryDetailResponse newCategoryDetailResponse = new CategoryDetailResponse();
  private CategoryDetailResponse oldCategoryDetailResponse = new CategoryDetailResponse();
  private ProductCenterHistory productCenterHistory;

  @Captor
  private ArgumentCaptor<ProductCenterHistory> productCenterHistoryArgumentCaptor;

  @Mock
  private ProductCenterHistoryRepository productCenterHistoryRepository;

  @InjectMocks
  private  ProductCenterHistoryServiceImpl productCenterHistoryService;

  @BeforeEach
  public void init() {
    openMocks(this);
    newCategoryDetailResponse.setName(NEW_CATEGORY_NAME);
    oldCategoryDetailResponse.setName(OLD_CATEGORY_NAME);
    productCenterHistory =
        ProductCenterHistory.builder().productSku(PRODUCT_SKU).activity(ProductCenterActivity.COPY.name())
            .description(ProductCenterActivity.COPY.getDescription()).build();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.productCenterHistoryRepository);
  }

  @Test
  public void saveProductCenterHistoryTest_copyActivity() {
    Mockito.when(this.productCenterHistoryRepository.save(Mockito.any(ProductCenterHistory.class)))
        .thenReturn(new ProductCenterHistory());
    this.productCenterHistoryService
        .saveProductCenterHistory(PRODUCT_SKU, ProductCenterActivity.COPY, REQUEST_ID,
            oldCategoryDetailResponse.getName(), newCategoryDetailResponse.getName());
    Mockito.verify(this.productCenterHistoryRepository).save(productCenterHistoryArgumentCaptor.capture());
   Assertions.assertEquals(STORE_ID, productCenterHistoryArgumentCaptor.getValue().getStoreId());
   Assertions.assertEquals(ProductCenterActivity.COPY.getDescription(),
        productCenterHistoryArgumentCaptor.getValue().getActivity());
   Assertions.assertEquals(REQUEST_ID, productCenterHistoryArgumentCaptor.getValue().getUpdatedBy());
  }

  @Test
  public void saveProductCenterHistoryTestUpdateEtdNotesActivity() {
    Mockito.when(this.productCenterHistoryRepository.save(Mockito.any(ProductCenterHistory.class)))
        .thenReturn(new ProductCenterHistory());
    this.productCenterHistoryService
        .saveProductCenterHistory(PRODUCT_SKU, ProductCenterActivity.ETD_NOTES_UPDATE, REQUEST_ID,
            oldCategoryDetailResponse.getName(), newCategoryDetailResponse.getName());
    Mockito.verify(this.productCenterHistoryRepository).save(productCenterHistoryArgumentCaptor.capture());
   Assertions.assertEquals(STORE_ID, productCenterHistoryArgumentCaptor.getValue().getStoreId());
   Assertions.assertEquals(ProductCenterActivity.ETD_NOTES_UPDATE.getDescription(),
        productCenterHistoryArgumentCaptor.getValue().getActivity());
   Assertions.assertEquals(REQUEST_ID, productCenterHistoryArgumentCaptor.getValue().getUpdatedBy());
  }

  @Test
  public void saveProductCenterHistoryTest_moveActivity() {
    Mockito.when(this.productCenterHistoryRepository.save(Mockito.any(ProductCenterHistory.class)))
        .thenReturn(new ProductCenterHistory());
    this.productCenterHistoryService
        .saveProductCenterHistory(PRODUCT_SKU, ProductCenterActivity.MOVE, REQUEST_ID,
            oldCategoryDetailResponse.getName(), newCategoryDetailResponse.getName());
    Mockito.verify(this.productCenterHistoryRepository).save(productCenterHistoryArgumentCaptor.capture());
   Assertions.assertEquals(STORE_ID, productCenterHistoryArgumentCaptor.getValue().getStoreId());
   Assertions.assertEquals(ProductCenterActivity.MOVE.getDescription(),
        productCenterHistoryArgumentCaptor.getValue().getActivity());
   Assertions.assertEquals(REQUEST_ID, productCenterHistoryArgumentCaptor.getValue().getUpdatedBy());
  }

  @Test
  public void saveProductCenterHistoryTest_deleteActivity() {
    Mockito.when(this.productCenterHistoryRepository.save(Mockito.any(ProductCenterHistory.class)))
        .thenReturn(new ProductCenterHistory());
    this.productCenterHistoryService
        .saveProductCenterHistory(PRODUCT_SKU, ProductCenterActivity.DELETE, REQUEST_ID,
            oldCategoryDetailResponse.getName(), newCategoryDetailResponse.getName());
    Mockito.verify(this.productCenterHistoryRepository).save(productCenterHistoryArgumentCaptor.capture());
   Assertions.assertEquals(STORE_ID, productCenterHistoryArgumentCaptor.getValue().getStoreId());
   Assertions.assertEquals(ProductCenterActivity.DELETE.getDescription(),
        productCenterHistoryArgumentCaptor.getValue().getActivity());
   Assertions.assertEquals(REQUEST_ID, productCenterHistoryArgumentCaptor.getValue().getUpdatedBy());
  }

  @Test
  public void getProductCenterHistoryByStoreIdAndProductSkuTest() {
    Pageable pageable = PageRequest.of(page, size);
    Mockito.when(this.productCenterHistoryRepository
        .findProductCenterHistoryByStoreIdAndProductSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(STORE_ID, PRODUCT_SKU, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(productCenterHistory), pageable, size));
    Page<ProductCenterHistoryResponse> response =
        productCenterHistoryService.getProductCenterHistoryByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, page, size);
    Mockito.verify(this.productCenterHistoryRepository)
        .findProductCenterHistoryByStoreIdAndProductSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(STORE_ID, PRODUCT_SKU, pageable);
   Assertions.assertEquals(PRODUCT_SKU, response.getContent().get(0).getProductSku());
   Assertions.assertEquals(ProductCenterActivity.COPY.name(), response.getContent().get(0).getActivity());
  }
}
