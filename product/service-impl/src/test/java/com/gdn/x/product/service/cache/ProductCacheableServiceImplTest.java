package com.gdn.x.product.service.cache;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.context.ApplicationContext;
import org.springframework.data.redis.core.BoundValueOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.ProductCacheableService;

/**
 * This unit test class template is the work of maria.o.a.sunaryo and felix.w.wijaya as
 * participation in Blibli Hackathon #1. Generated on 23 Jun 2016 23:04:30
 */
public class ProductCacheableServiceImplTest {

  private static final String STORE_ID = "storeId";

  private static final String PRODUCT_SKU = "productSku";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String CATEGORY_CODE = "categoryCode";

  @InjectMocks
  private ProductCacheableServiceImpl sut;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private RedisTemplate<String, Object> productRedisTemplate;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private BoundValueOperations boundValueOperations;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;


  @Test
  public void findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrueRedisEnabledTrue2Test() {
    ReflectionTestUtils.setField(sut, "redisProductEnabled", true);
    String storeId = "store-id";
    String productCode = "product-code";
    Product product = new Product();
    product.setProductName("product-name");
    product.setSynchronized(true);
    List<Product> productList = new ArrayList<>();
    productList.add(product);
    Set<String> productCodes = new HashSet<String>();
    productCodes.add(productCode);
    List<Product> syncProducts = new ArrayList<>();
    Mockito.when(productRedisTemplate.boundValueOps(Mockito.anyString())).thenReturn(boundValueOperations);
    Mockito.when(boundValueOperations.get()).thenReturn(productList);
    List<Product> productList1 =
        this.sut.findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(storeId, productCode);
    for (Product product1 : productList) {
      product1.setSynchronized(true);
      syncProducts.add(product1);
    }
    Mockito.verify(productRedisTemplate, times(1)).boundValueOps(Mockito.anyString());
    Mockito.verify(boundValueOperations).get();
  }

  @Test
  public void findByStoreIdAndProductCodeAndMarkForDeleteFalseTest() {
    ReflectionTestUtils.setField(sut, "redisProductEnabled", false);
    String storeId = "store-id";
    String productCode = "product-code";
    Set<String> productCodes = new HashSet<String>();
    productCodes.add(productCode);
    Product product = new Product();
    Mockito.when(
        productRepository.findByStoreIdAndProductCodeInAndMarkForDeleteFalse(storeId, productCodes))
        .thenReturn(Arrays.asList(product));
    List<Product> products =
        this.sut.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    verify(this.productRepository)
        .findByStoreIdAndProductCodeInAndMarkForDeleteFalse(storeId, productCodes);
    Assertions.assertEquals(product, products.get(0));
  }

  @Test
  public void findByStoreIdAndProductCodeAndMarkForDeleteFalseTestRedisEnabledTrueTest() {
    ReflectionTestUtils.setField(sut, "redisProductEnabled", true);
    String storeId = "store-id";
    String productCode = "product-code";
    Set<String> productCodes = new HashSet<String>();
    productCodes.add(productCode);
    Mockito.when(productRedisTemplate.boundValueOps(Mockito.anyString())).thenReturn(boundValueOperations);
    Mockito.when(boundValueOperations.get()).thenReturn(Arrays.asList(new Product()));
    this.sut.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    Mockito.verify(productRedisTemplate, times(1)).boundValueOps(Mockito.anyString());
    Mockito.verify(boundValueOperations).get();
  }

  @Test
  public void findByStoreIdAndProductCodeAndMarkForDeleteFalseTestRedisEnabledTrue2Test() {
    ReflectionTestUtils.setField(sut, "redisProductEnabled", true);
    ReflectionTestUtils.setField(sut, "redisProductExpiration", "86400");
    String storeId = "store-id";
    String productCode = "product-code";
    Set<String> productCodes = new HashSet<String>();
    productCodes.add(productCode);
    Product product = new Product();
    Mockito.when(productRedisTemplate.boundValueOps(Mockito.anyString())).thenReturn(boundValueOperations);
    Mockito.when(boundValueOperations.get()).thenReturn(new ArrayList<>());
    Mockito.when(
        productRepository.findByStoreIdAndProductCodeInAndMarkForDeleteFalse(storeId, productCodes))
        .thenReturn(Arrays.asList(product));
    Mockito.when(productRedisTemplate.expire(Mockito.any(), eq(86400), eq(TimeUnit.SECONDS))).thenReturn(Boolean.TRUE);
    Mockito.doNothing().when(boundValueOperations).set(new ArrayList<>());
    this.sut.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    Mockito.verify(productRedisTemplate, times(2)).boundValueOps(Mockito.anyString());
    verify(this.productRepository)
        .findByStoreIdAndProductCodeInAndMarkForDeleteFalse(storeId, productCodes);
    Mockito.verify(boundValueOperations).set(Mockito.any());
    Mockito.verify(boundValueOperations).get();
    Mockito.verify(productRedisTemplate).expire(Mockito.any(), eq(Long.valueOf("86400")), eq(TimeUnit.SECONDS));
  }

  @Test
  public void findProductByStoreIdAndProductSkuAndMarkForDeleteFalseTest() {
    String storeId = "234";
    String productSku = "12345";
    Product product = new Product();
    product.setProductSku("12345");
    Mockito.when(applicationContext.getBean(ProductCacheableService.class)).thenReturn(sut);
    Mockito.when(productRepository.findProductByStoreIdAndProductSku(storeId, productSku, false)).thenReturn(product);
    Product result = this.sut.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    verify(applicationContext).getBean(ProductCacheableService.class);
    verify(this.productRepository).findProductByStoreIdAndProductSku(storeId, productSku, false);
    Assertions.assertEquals(product, result);
  }

  @Test
  public void findProductByStoreIdAndProductSkuAndIncludeMarkForDeleteTest() {
    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    Mockito.when(applicationContext.getBean(ProductCacheableService.class)).thenReturn(sut);
    Mockito.when(productRepository.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false)).thenReturn(product);
    Product result = this.sut.findProductByStoreIdAndProductSkuAndIncludeMarkForDelete(STORE_ID, PRODUCT_SKU, true);
    verify(applicationContext).getBean(ProductCacheableService.class);
    verify(this.productRepository).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false);
    Assertions.assertEquals(product, result);
  }

  @Test
  public void findProductByStoreIdAndProductSkuAndIncludeMarkForDeleteProductNullTest() {
    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    Mockito.when(applicationContext.getBean(ProductCacheableService.class)).thenReturn(sut);
    Mockito.when(productRepository.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false)).thenReturn(null);
    Product result = this.sut.findProductByStoreIdAndProductSkuAndIncludeMarkForDelete(STORE_ID, PRODUCT_SKU, true);
    verify(applicationContext).getBean(ProductCacheableService.class);
    verify(this.productRepository).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false);
    Assertions.assertNull(result);
  }

  @Test
  public void findProductByStoreIdAndProductSkuAndIncludeMarkForDeleteIncludeMfdFalseTest() {
    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    Mockito.when(applicationContext.getBean(ProductCacheableService.class)).thenReturn(sut);
    Mockito.when(productRepository.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false)).thenReturn(product);
    Product result = this.sut.findProductByStoreIdAndProductSkuAndIncludeMarkForDelete(STORE_ID, PRODUCT_SKU, false);
    verify(applicationContext).getBean(ProductCacheableService.class);
    verify(this.productRepository).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false);
    Assertions.assertEquals(product, result);
  }

  @Test
  public void findProductByStoreIdAndProductSkuAndIncludeMarkForDeleteIncludeMfdTrueTest() {
    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    product.setMarkForDelete(true);
    Mockito.when(applicationContext.getBean(ProductCacheableService.class)).thenReturn(sut);
    Mockito.when(productRepository.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false)).thenReturn(product);
    Product result = this.sut.findProductByStoreIdAndProductSkuAndIncludeMarkForDelete(STORE_ID, PRODUCT_SKU, false);
    verify(applicationContext).getBean(ProductCacheableService.class);
    verify(this.productRepository).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false);
    Assertions.assertNull(result);
  }

  @Test
  public void findProductByStoreIdAndProductSkuAndMarkForDeleteTrueTest() {
    String storeId = "234";
    String productSku = "12345";
    Product product = new Product();
    product.setProductSku("12345");
    product.setMarkForDelete(true);
    Mockito.when(applicationContext.getBean(ProductCacheableService.class)).thenReturn(sut);
    Mockito.when(productRepository.findProductByStoreIdAndProductSku(storeId, productSku, false)).thenReturn(product);
    Product result = this.sut.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    verify(applicationContext).getBean(ProductCacheableService.class);
    verify(this.productRepository).findProductByStoreIdAndProductSku(storeId, productSku, false);
    Assertions.assertEquals(null, result);
  }


  @Test
  public void findProductByStoreIdAndProductSkuTest() {
    String storeId = "1234";
    String productSku = "12345";
    Product product = new Product();
    Mockito.when(productRepository.findProductByStoreIdAndProductSku(storeId, productSku, false))
        .thenReturn(product);
    Product result = this.sut.findProductByStoreIdAndProductSku(storeId, productSku);
    verify(this.productRepository).findProductByStoreIdAndProductSku(storeId, productSku, false);
    Assertions.assertEquals(product, result);
  }

  @Test
  public void findProductsByStoreIdAndProductSkusAndMarkForDeleteFalseTest(){
    Map<String, Product> expectedResult = new HashMap<>();
    List<Product> products = new ArrayList<>();
    Product product = new Product(PRODUCT_SKU);
    expectedResult.put(PRODUCT_SKU, product);
    products.add(product);
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    when(productRepository.findProductByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID,
        productSkus)).thenReturn(products);
    Map<String, Product> result =
        sut.findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(STORE_ID, productSkus);
    Assertions.assertEquals(expectedResult, result);
    verify(productRepository).findProductByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID,
        productSkus);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.productRepository, productRedisTemplate, boundValueOperations);
  }

  @Test
  public void getCategoryCodesCachedByAttributeCodeTest() {
    Mockito.when(
            productCategoryBaseOutbound.getCategoryCodesByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(Collections.singletonList(ATTRIBUTE_CODE));
    List<String> categoryCodes =
        sut.getCategoryCodesCachedByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    Assertions.assertFalse(categoryCodes.isEmpty());
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryCodesByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
  }
}
