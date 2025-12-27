package com.gdn.partners.product.analytics.service.impl.cache;

import com.gdn.partners.product.analytics.repository.ProductOptimisationRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ProductOptimisationCacheableServiceTest {

  private static final String SELLER_CODE = "sellerCode";

  @InjectMocks
  private ProductOptimisationCacheableServiceImpl productOptimisationCacheableService;

  @Mock
  private ProductOptimisationRepository productOptimisationRepository;

  @BeforeEach
  void setUp() {}

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(productOptimisationRepository);
  }

  @Test
  void getProductCount() {
    Mockito.when(productOptimisationRepository.countBySellerCodeAndMarkForDeleteFalse(SELLER_CODE)).thenReturn(
        1L);
    productOptimisationCacheableService.findProductCountCacheablesBySellerCode(SELLER_CODE);
    Mockito.verify(productOptimisationRepository).countBySellerCodeAndMarkForDeleteFalse(SELLER_CODE);
  }

  @Test
  void evictProductCountCacheablesBySellerCode() {
    productOptimisationCacheableService.evictCacheBySellerCode(SELLER_CODE);
  }
}
