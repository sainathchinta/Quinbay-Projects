package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import com.gdn.x.productcategorybase.dto.ProductItemDTO;
import com.gdn.x.productcategorybase.dto.ProductItemImageDTO;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.common.util.BeanUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.repository.ProductItemRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.ProductCategoryService;
import com.gdn.x.productcategorybase.service.ProductItemAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.solr.dao.SolrProductDao;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public class ProductItemServiceTest {
  private static final String PRODUCT_CODE = "product_code";
  private static final String STORE_ID = "STORE_ID";
  private static final String UPC_CODE = "UPC_CODE";
  private static final String SKU_CODE = "SKU_CODE";
  private static final String WRONG_SKU_CODE = "WRONG_SKU_CODE";
  private static final String GENERATED_ITEM_NAME = "ITEM_NAME";
  private static final byte[] HASH = "HASH_CODE".getBytes();
  private static final String UPC_CODE_1 = "UPC_CODE_1";
  private static final String SKU_CODE_1 = "SKU_CODE1";
  private static final String GENERATED_ITEM_NAME_1 = "ITEM_NAME_1";
  private static final byte[] HASH_1 = "HASH_CODE_1".getBytes();
  private static final String WRONG_ID = "WRONG_ID";
  private static final String ITEM_NAME_SEARCH = "item_name";
  private static final String UPC_CODE_SEARCH = "upc_code";
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final String PRODUCT_CODE_START_WITH_UK = "UK-234567";
  private static final Pageable DEFAULT_PAGEABLE = PageRequest.of(0, 10);
  private static final int solrStart = 0;
  private static final int solrRows = 10;
  private static final int TOTAL_RECORDS = 1;
  private static final String CATEGORY_ID = "CATEGORY_ID";
  private static final String NEW_LOCATION_PATH = "newLocationPath";
  private static final String CATEGORY_CODE = "CATEGORY_CODE";
  private static final String productId = UUID.randomUUID().toString();
  private static final String PRODUCT_ID = "id";
  private static final String PRODUCT_ITEM_ID_1 = "productItemId1";
  private static final String PRODUCT_ITEM_ID_2 = "productItemId2";
  private static final String ITEM_SKU_CODE_1 = "itemSkuCode1";
  private static final String ITEM_SKU_CODE_2 = "itemSkuCode2";
  private static final String PRODUCT_ITEM_ATTRIBUTE_VALUE_1 = "productItemAttributeValue1";
  private static final String PRODUCT_ITEM_ATTRIBUTE_VALUE_2 = "productItemAttributeValue2";
  private static final String PRODUCT_ITEM_ATTRIBUTE_VALUE_3 = "productItemAttributeValue3";
  private static final String PRODUCT_ITEM_NAME_1 = "productItemName";
  private static final String ATTRIBUTE_ID_1 = "attributeId1";
  private static final String ATTRIBUTE_ID_2 = "attributeId2";
  private static final String ATTRIBUTE_ID_3 = "attributeId3";
  private static final String ATTRIBUTE_NAME_1 = "attributeName1";
  private static final String ATTRIBUTE_NAME_2 = "attributeName2";
  private static final String ATTRIBUTE_NAME_3 = "attributeName3";
  private static final String ATTRIBUTE_CODE_1 = "ATTR-0000001";
  private static final String ATTRIBUTE_CODE_2 = "ATTR-0000002";
  private static final String ATTRIBUTE_CODE_3 = "ATTR-0000003";
  private static final String IMAGE_ID_1 = "imageId1";
  private static final String IMAGE_ID_2 = "imageId2";
  private static final String IMAGE_ID_3 = "imageId3";

  @Mock
  private ProductItemRepository repository;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductCategoryService productCategoryService;

  @Mock
  private CategoryService categoryService;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;
  
  @Mock
  private Page<String> page;

  @Mock
  private Page<Object[]> pageOfObject;

  @Mock
  private SolrProductDao solrProductDao;

  @Mock
  private ImageService imageService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductItemAttributeValueService productItemAttributeValueService;

  @Mock
  private AttributeService attributeService;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @InjectMocks
  ProductItemServiceBean service;

  private List<ProductItem> productItems;
  private List<ProductItem> emptyProductItems;
  private Page<ProductItem> productItemsPage;
  private Page<ProductItem> productItemsPage1;
  private ProductItem productItem;
  private ProductItem productItem1;
  private ProductItem productItemActive;
  Product product;
  private List<String> skuCodes;
  private String skuCodesRegex;
  private ProductItemImage productItemImage;
  private ProductItemImage productItemImage1;
  private ProductItemImage productItemImage2;
  private ProductItemImage productItemImage3;
  private ProductItemAttributeValue productItemAttributeValue1;
  private ProductItemAttributeValue productItemAttributeValue2;
  private ProductItemAttributeValue productItemAttributeValue3;

  @Test
  public void activateProductItemSingleTest() throws Exception {
    when(
        this.repository.findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
            ProductItemServiceTest.UPC_CODE, true)).thenReturn(this.emptyProductItems);
    when(this.repository.findById(this.productItem.getId())).thenReturn(Optional.of(this.productItem));

    this.service.activateProductItem(ProductItemServiceTest.STORE_ID, this.productItem);
    assertEquals(this.productItem.isActivated(), (true));

    // verify(repository,
    // times(1)).findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(STORE_ID, UPC_CODE, true);
    verify(this.repository, times(1)).findById(this.productItem.getId());
    verify(this.repository, times(1)).saveAndFlush(this.productItem);
  }

  @Test
  public void activateProductItemWithEmptySkuTest() throws Exception {
    this.productItem.setSkuCode("");
    this.service.activateProductItem(ProductItemServiceTest.STORE_ID, this.productItem);
  }

  @Test
  public void activateProductItemWithEmptyUpcTest() throws Exception {
    this.productItem.setUpcCode("");
    this.service.activateProductItem(ProductItemServiceTest.STORE_ID, this.productItem);
  }

  @Test
  public void activateProductItemWithNullSkuTest() throws Exception {
    this.productItem.setSkuCode(null);
    this.service.activateProductItem(ProductItemServiceTest.STORE_ID, this.productItem);
  }

  @Test
  public void activateProductItemWithNullUpcTest() throws Exception {
    this.productItem.setUpcCode(null);
    this.service.activateProductItem(ProductItemServiceTest.STORE_ID, this.productItem);
  }

  @Test
  public void countBySkuCodeTest() throws Exception {
    long count = this.service.countBySkuCode(ProductItemServiceTest.STORE_ID, ProductItemServiceTest.SKU_CODE);

    assertEquals(count, 1);

    count = this.service.countBySkuCode(ProductItemServiceTest.STORE_ID, ProductItemServiceTest.WRONG_SKU_CODE);

    assertEquals(count, 0);

    verify(this.repository, times(1)).countByStoreIdAndSkuCode(ProductItemServiceTest.STORE_ID,
        ProductItemServiceTest.SKU_CODE);
    verify(this.repository, times(1)).countByStoreIdAndSkuCode(ProductItemServiceTest.STORE_ID,
        ProductItemServiceTest.WRONG_SKU_CODE);
  }

  /*
   * this line of code delete because the system now allow duplicate UPC_CODE
   * @Test public void activateProductItemWithSameUpcTest() throws Exception { when(repository
   * .findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(STORE_ID, UPC_CODE, true))
   * .thenReturn(productItems); try { service.activateProductItem(STORE_ID, productItem); } catch
   * (Exception e) { assertTrue(e instanceof ApplicationException); ApplicationException
   * applicationException = (ApplicationException) e; System.out.println(e.getMessage()); if
   * (applicationException.getErrorMessage().contains(
   * String.format(ProductItemServiceBean.PRODUCT_ITEM_WITH_SAME_UPC_EXIST, UPC_CODE))) {
   * assertTrue(true); } else { assertTrue(false); } verify(repository, times(1))
   * .findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(STORE_ID, UPC_CODE, true); } }
   */

  @Test
  public void deactivateListProductItemTest() throws Exception {
    this.productItem.setActivated(true);
    this.productItem1.setActivated(true);
    when(
        this.repository.findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
            ProductItemServiceTest.UPC_CODE, true)).thenReturn(this.emptyProductItems);
    when(
        this.repository.findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
            ProductItemServiceTest.UPC_CODE_1, true)).thenReturn(this.emptyProductItems);
    when(this.repository.findById(this.productItem.getId())).thenReturn(Optional.of(this.productItem));
    when(this.repository.findById(this.productItem1.getId())).thenReturn(Optional.of(this.productItem1));

    this.service.deactivateProductItem(ProductItemServiceTest.STORE_ID, this.productItems);
    assertEquals(this.productItem.isActivated(), (false));
    assertEquals(this.productItem1.isActivated(), (false));

    // verify(repository,
    // times(1)).findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(STORE_ID, UPC_CODE,
    // false);
    verify(this.repository, times(1)).findById(this.productItem.getId());
    verify(this.repository, times(1)).saveAndFlush(this.productItem);
    // verify(repository,
    // times(1)).findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(STORE_ID, UPC_CODE_1,
    // false);
    verify(this.repository, times(1)).findById(this.productItem1.getId());
    verify(this.repository, times(1)).saveAndFlush(this.productItem1);
  }

  @Test
  public void deactivateSingleProductItemTest() throws Exception {
    this.productItem.setActivated(true);
    when(this.repository.findById(this.productItem.getId())).thenReturn(Optional.of(this.productItem));
    when(
        this.repository.findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
            ProductItemServiceTest.UPC_CODE, true)).thenReturn(this.emptyProductItems);

    this.service.deactivateProductItem(ProductItemServiceTest.STORE_ID, this.productItem);
    assertEquals(this.productItem.isActivated(), (false));

    // verify(repository,
    // times(1)).findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(STORE_ID, UPC_CODE,
    // false);
    verify(this.repository, times(1)).findById(this.productItem.getId());
    verify(this.repository, times(1)).saveAndFlush(this.productItem);
  }

  @Test
  public void deleteNonExistenceProductIdTest() throws Exception {
    when(this.repository.findById(ProductItemServiceTest.WRONG_ID)).thenReturn(Optional.ofNullable(null));

    try {
      this.service.delete(ProductItemServiceTest.WRONG_ID);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ErrorCategory.DATA_NOT_FOUND.getMessage())) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
      verify(this.repository, times(1)).findById(ProductItemServiceTest.WRONG_ID);
    }
  }

  @Test
  public void deleteNullProductIdTest() throws Exception {
    try {
      this.service.delete(null);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ErrorCategory.DATA_NOT_FOUND.getMessage())) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
    }
  }

  @Test
  public void deleteTest() throws Exception {
    when(this.repository.findById(this.productItem.getId())).thenReturn(Optional.of(this.productItem));
    doNothing().when(this.repository).deleteById(this.productItem.getId());

    this.service.delete(this.productItem.getId());

    verify(this.repository, times(2)).findById(this.productItem.getId());
    verify(this.repository, times(1)).delete(this.productItem);
  }

  @Test
  public void findByIdTest() throws Exception {
    when(this.repository.findById(this.productItem.getId())).thenReturn(Optional.of(this.productItem));
    ProductItem savedProductItem = this.service.findById(this.productItem.getId());
    assertEquals(savedProductItem, (this.productItem));
    verify(this.repository, times(1)).findById(this.productItem.getId());
  }

  @Test
  public void findByMultipleUpcCodeExcludeOneItemTest() throws Exception {
    Page<ProductItem> result =
        this.service.findByMultipleUpcCodeExcludeOneItem(ProductItemServiceTest.STORE_ID, this.skuCodes,
            ProductItemServiceTest.SKU_CODE_1, ProductItemServiceTest.DEFAULT_PAGEABLE);

    assertEquals(this.productItemsPage1, result);

    verify(this.repository, times(1)).findByStoreIdAndUpcCodeExcludeOneItemMatches(ProductItemServiceTest.STORE_ID,
        this.skuCodesRegex, ProductItemServiceTest.SKU_CODE_1, ProductItemServiceTest.DEFAULT_PAGEABLE);
    verify(productService).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
  }

  @Test
  public void findByMultipleUpcCodeTest() throws Exception {
    Page<ProductItem> result =
        this.service.findByMultipleUpcCode(ProductItemServiceTest.STORE_ID, this.skuCodes,
            ProductItemServiceTest.DEFAULT_PAGEABLE);

    assertEquals(this.productItemsPage, result);

    verify(this.repository, times(1)).findByStoreIdAndUpcCodeMatches(ProductItemServiceTest.STORE_ID,
        this.skuCodesRegex, ProductItemServiceTest.DEFAULT_PAGEABLE);
    verify(productService, times(2))
        .getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
  }
  
	@Test
	public void testFindDuplicateProductItemsOrByUpcCode() throws Exception {
		when(this.repository
				.findByStoreIdAndGeneratedItemNameContainingIgnoreCaseAndUpcCodeIsNotNullOrUpcCodeAndMarkForDeleteFalse(
						ProductItemServiceTest.STORE_ID, ProductItemServiceTest.PRODUCT_NAME,
						ProductItemServiceTest.UPC_CODE, ProductItemServiceTest.DEFAULT_PAGEABLE)).thenReturn(pageOfObject);
		this.service.findDuplicateProductItemsOrByUpcCode(ProductItemServiceTest.STORE_ID,
				ProductItemServiceTest.UPC_CODE, ProductItemServiceTest.PRODUCT_NAME,
				ProductItemServiceTest.DEFAULT_PAGEABLE);
		verify(this.repository)
				.findByStoreIdAndGeneratedItemNameContainingIgnoreCaseAndUpcCodeIsNotNullOrUpcCodeAndMarkForDeleteFalse(
						ProductItemServiceTest.STORE_ID, ProductItemServiceTest.PRODUCT_NAME,
						ProductItemServiceTest.UPC_CODE, ProductItemServiceTest.DEFAULT_PAGEABLE);
	}

	@Test
	public void findByStoreIdAndIdTest() throws Exception {
		when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
				this.productItem.getId())).thenReturn(this.productItem);
		ProductItem savedProductItem = this.service.findByStoreIdAndId(ProductItemServiceTest.STORE_ID,
				this.productItem.getId());
		assertEquals(savedProductItem, (this.productItem));
		verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
				this.productItem.getId());
    verify(productService).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
	}

  @Test
  public void findByStoreIdAndIdAndMarkForDeleteFalseTest() throws Exception {
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
      this.productItem.getId())).thenReturn(this.productItem);
    ProductItem savedProductItem =
      this.service.findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
        this.productItem.getId());
    assertEquals(savedProductItem, (this.productItem));
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
      this.productItem.getId());
  }

  @Test
  public void findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalseTest() {
    service.findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(STORE_ID, SKU_CODE,
        Collections.singletonList(SKU_CODE_1));
    verify(repository).findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(STORE_ID, SKU_CODE,
        Collections.singletonList(SKU_CODE_1));
  }

  @Test
  public void findByStoreIdAndSkuCodeTest() throws Exception {
    productItem.setProductItemAttributeValues(Arrays.asList(productItemAttributeValue1, productItemAttributeValue2));
    ProductItem result =
        this.service.findByStoreIdAndSkuCode(ProductItemServiceTest.STORE_ID, ProductItemServiceTest.SKU_CODE);
    assertEquals(this.productItem, result);

    verify(this.repository, times(1)).findByStoreIdAndSkuCodeAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
        ProductItemServiceTest.SKU_CODE);
    verify(productService).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
  }

  @Test
  public void findByStoreIdAndSkuCodeWithEmptyProductItemTest() throws Exception {
    try {
      this.service.findByStoreIdAndSkuCode(ProductItemServiceTest.STORE_ID, ProductItemServiceTest.SKU_CODE + "1");
    } catch (Exception e) {
      assertTrue(e.getMessage().contains("not found product item with skuCode"));
      verify(this.repository, times(1)).findByStoreIdAndSkuCodeAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
          ProductItemServiceTest.SKU_CODE + "1");
    }
  }

  @Test
  public void findByStoreIdAndViewableAndGeneratedItemNameOrUpcCodeTest() {
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    Pageable pageable = PageRequest.of(0, 10);
    productItems.add(this.productItem);
    Page<ProductItem> productPage =
        new PageImpl<ProductItem>(productItems, ProductItemServiceTest.DEFAULT_PAGEABLE,
            productItems.size());
    when(this.repository.findProductItemList(Mockito.eq(ProductItemServiceTest.STORE_ID),
        Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(PageRequest.class))).thenReturn(productPage);
    Assertions.assertEquals(productPage,
        service.findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode(STORE_ID, true, true,
            UPC_CODE, pageable));
    verify(this.repository).findProductItemList(Mockito.eq(ProductItemServiceTest.STORE_ID),
        Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(PageRequest.class));
    verify(this.productService)
        .getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productItem.getProductId());
  }

  @Test
  public void findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode_FalseTest() {
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    Pageable pageable = PageRequest.of(0, 10);
    productItems.add(this.productItem);
    Page<ProductItem> productPage =
        new PageImpl<ProductItem>(productItems, ProductItemServiceTest.DEFAULT_PAGEABLE,
            productItems.size());
    when(this.repository.findAllProductItemList(Mockito.eq(ProductItemServiceTest.STORE_ID),
        Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(PageRequest.class))).thenReturn(productPage);
    Assertions.assertEquals(productPage,
        service.findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode(STORE_ID, true, false,
            UPC_CODE, pageable));
    verify(this.repository).findAllProductItemList(Mockito.eq(ProductItemServiceTest.STORE_ID),
        Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(PageRequest.class));
    verify(this.productService)
        .getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productItem.getProductId());
  }

  @Test
  public void findByStoreIdTest() throws Exception {
    Page<ProductItem> productPage =
        new PageImpl<ProductItem>(this.productItems, ProductItemServiceTest.DEFAULT_PAGEABLE, this.productItems.size());
    when(
        this.repository.findByStoreIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
            ProductItemServiceTest.DEFAULT_PAGEABLE)).thenReturn(productPage);
    this.service.findByStoreId(ProductItemServiceTest.STORE_ID, ProductItemServiceTest.DEFAULT_PAGEABLE);
    verify(this.repository, times(1)).findByStoreIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
        ProductItemServiceTest.DEFAULT_PAGEABLE);
    verify(productService, times(2))
        .getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
  }

  @Test
  public void getSequenceTest() throws Exception {
    Long result = 1L;
    String expectedResult = "00001";

    when(this.repository.getSequenceByProductCode(ProductItemServiceTest.PRODUCT_CODE)).thenReturn(result);
    assertEquals(this.service.getSequence(ProductItemServiceTest.PRODUCT_CODE), (expectedResult));
    verify(this.repository).getSequenceByProductCode(ProductItemServiceTest.PRODUCT_CODE);
  }

  @Test
  public void getSequenceTransactionTest() {
    Long result = 1L;
    when(this.repository.getSequenceByProductCode(ProductItemServiceTest.PRODUCT_CODE)).thenReturn(result);
    assertEquals("00001", this.service.getSequenceTransaction(ProductItemServiceTest.PRODUCT_CODE));
    verify(this.repository).getSequenceByProductCode(ProductItemServiceTest.PRODUCT_CODE);
  }

  @Test
  public void saveProductItemDetailTest_whenProductCodeStartsWithMerchant() throws Exception {
    String productItemId = UUID.randomUUID().toString();
    String productCode = "code";
    ProductItem savedProductItem = new ProductItem();
    savedProductItem.setId(productItemId);
    savedProductItem.setUpcCode(ProductItemServiceTest.UPC_CODE_1);
    List<ProductItemImage> productItemImages = new ArrayList<ProductItemImage>();
    List<ProductItemImage> secondItemImages = new ArrayList<ProductItemImage>();
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setId("1234");
    productItemImage.setMarkForDelete(false);
    productItemImages.add(productItemImage);
    secondItemImages.add(productItemImage);
    ProductItemImage secondProductImage = new ProductItemImage();
    secondProductImage.setId("12345");
    secondProductImage.setMarkForDelete(true);
    productItemImages.add(secondProductImage);
    secondItemImages.add(secondProductImage);
    ProductItemImage thirdProductImage = new ProductItemImage();
    thirdProductImage.setId("123456");
    thirdProductImage.setMarkForDelete(true);
    secondItemImages.add(thirdProductImage);
    savedProductItem.setProductItemImages(secondItemImages);
    Product parentProduct = new Product();
    parentProduct.setId(productId);
    parentProduct.setProductCode(productCode);
    List<ProductItem> savedProductItems = new ArrayList<>();
    savedProductItems.add(savedProductItem);
    parentProduct.setProductItems(savedProductItems);
    savedProductItem.setProduct(parentProduct);
    savedProductItem.setProductId(productId);
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    ProductItem productItem = new ProductItem();
    productItem.setId(productItemId);
    productItem.setSkuCode("MTA-234567-00001");
    productItem.setUpcCode(ProductItemServiceTest.UPC_CODE_1);
    productItem.setProduct(parentProduct);
    productItem.setProductItemImages(productItemImages);
    productItems.add(productItem);
    when(this.repository.findById(productItem.getId())).thenReturn(Optional.of(savedProductItem));
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID, productItemId))
        .thenReturn(savedProductItem);
    when(this.productRepository.findById(productId)).thenReturn(Optional.of(parentProduct));

    this.service.saveProductItemDetails(ProductItemServiceTest.STORE_ID, productItems, "MTA-12345");
    verify(this.productRepository).findById(productId);
    verify(this.productRepository, times(1)).saveAndFlush(parentProduct);
    verify(this.repository, times(1)).findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
        productItemId);
    verify(this.repository).getSequenceByProductCode(anyString());
  }

	@Test
	public void saveProductItemDetailTest() throws Exception {
		String productItemId = UUID.randomUUID().toString();
		String productId = UUID.randomUUID().toString();
		String productCode = "code";
		ProductItem savedProductItem = new ProductItem();
		savedProductItem.setId(productItemId);
		savedProductItem.setUpcCode(ProductItemServiceTest.UPC_CODE_1);
		List<ProductItemImage> productItemImages = new ArrayList<ProductItemImage>();
		List<ProductItemImage> secondItemImages = new ArrayList<ProductItemImage>();
		ProductItemImage productItemImage = new ProductItemImage();
		productItemImage.setId("1234");
		productItemImage.setMarkForDelete(false);
		productItemImages.add(productItemImage);
		secondItemImages.add(productItemImage);
		ProductItemImage secondProductImage = new ProductItemImage();
		secondProductImage.setId("12345");
		secondProductImage.setMarkForDelete(true);
		productItemImages.add(secondProductImage);
		secondItemImages.add(secondProductImage);
		ProductItemImage thirdProductImage = new ProductItemImage();
		thirdProductImage.setId("123456");
		thirdProductImage.setMarkForDelete(true);
		secondItemImages.add(thirdProductImage);
		savedProductItem.setProductItemImages(secondItemImages);
		Product parentProduct = new Product();
		parentProduct.setId(productId);
		parentProduct.setProductCode(productCode);
		savedProductItem.setProduct(parentProduct);
		savedProductItem.setProductId(productId);
		List<ProductItem> productItems = new ArrayList<ProductItem>();
		ProductItem productItem = new ProductItem();
		productItem.setId(productItemId);
    productItem.setSkuCode("MTA-234567-00001");
		productItem.setUpcCode(ProductItemServiceTest.UPC_CODE_1);
		productItem.setProduct(parentProduct);
		productItem.setProductItemImages(productItemImages);
		productItems.add(productItem);
    List<ProductItem> savedProductItems = new ArrayList<>();
    savedProductItems.add(productItem);
    productItem = new ProductItem();
    productItem.setId(productItemId);
    productItem.setSkuCode("MTA-12345-00001");
    productItem.setUpcCode(ProductItemServiceTest.UPC_CODE_1);
    productItem.setProduct(parentProduct);
    productItem.setProductId(productId);
    productItem.setProductItemImages(productItemImages);
    productItems.add(productItem);
    savedProductItems.add(savedProductItem);
    savedProductItems.add(productItem);
    parentProduct.setProductItems(savedProductItems);
		when(this.repository.findById(productItem.getId())).thenReturn(Optional.of(savedProductItem));
		when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID, productItemId))
				.thenReturn(savedProductItem);
		when(this.productRepository.findById(productId)).thenReturn(Optional.of(parentProduct));
    when(productService.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId))
        .thenReturn(parentProduct);
		this.service.saveProductItemDetails(ProductItemServiceTest.STORE_ID, productItems, "MTA-12345");
		verify(this.productRepository, times(2)).findById(productId);
		verify(this.productRepository, times(2)).saveAndFlush(parentProduct);
		verify(this.repository, times(2)).findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
				productItemId);
    verify(this.repository, times(3)).getSequenceByProductCode(anyString());
	}

  @Test
  public void saveProductItemDetailTestWithOldItem() throws Exception {
    String productItemId = UUID.randomUUID().toString();
    String productId = UUID.randomUUID().toString();
    String productCode = PRODUCT_CODE_START_WITH_UK;
    ProductItem savedProductItem = new ProductItem();
    savedProductItem.setId(productItemId);
    savedProductItem.setUpcCode(ProductItemServiceTest.UPC_CODE_1);
    List<ProductItemImage> productItemImages = new ArrayList<ProductItemImage>();
    List<ProductItemImage> secondItemImages = new ArrayList<ProductItemImage>();
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setId("1234");
    productItemImage.setMarkForDelete(false);
    productItemImages.add(productItemImage);
    secondItemImages.add(productItemImage);
    ProductItemImage secondProductImage = new ProductItemImage();
    secondProductImage.setId("12345");
    secondProductImage.setMarkForDelete(true);
    productItemImages.add(secondProductImage);
    secondItemImages.add(secondProductImage);
    ProductItemImage thirdProductImage = new ProductItemImage();
    thirdProductImage.setId("123456");
    thirdProductImage.setMarkForDelete(true);
    secondItemImages.add(thirdProductImage);
    savedProductItem.setProductItemImages(secondItemImages);
    Product parentProduct = new Product();
    parentProduct.setId(productId);
    parentProduct.setProductCode(productCode);
    savedProductItem.setProduct(parentProduct);
    savedProductItem.setProductId(productId);
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    ProductItem productItem = new ProductItem();
    productItem.setId(productItemId);
    productItem.setSkuCode(PRODUCT_CODE_START_WITH_UK);
    productItem.setUpcCode(ProductItemServiceTest.UPC_CODE_1);
    productItem.setProduct(parentProduct);
    productItem.setProductItemImages(productItemImages);
    productItems.add(productItem);
    List<ProductItem> savedProductItems = new ArrayList<>();
    savedProductItems.add(productItem);
    savedProductItems.add(savedProductItem);
    savedProductItems.add(productItem);
    parentProduct.setProductItems(savedProductItems);
    when(this.repository.findById(productItem.getId())).thenReturn(Optional.of(savedProductItem));
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID, productItemId))
        .thenReturn(savedProductItem);
    when(this.productRepository.findById(productId)).thenReturn(Optional.of(parentProduct));
    this.service.saveProductItemDetails(ProductItemServiceTest.STORE_ID, productItems, PRODUCT_CODE_START_WITH_UK);
    verify(this.productRepository).findById(productId);
    verify(this.productRepository).saveAndFlush(parentProduct);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
        productItemId);
  }

  @Test
  public void saveProductItemDetailTest1() throws Exception {
    String productItemId = UUID.randomUUID().toString();
    String productId = UUID.randomUUID().toString();
    String productCode = "code";
    ProductItem savedProductItem1 = new ProductItem();
    savedProductItem1.setId("1");
    savedProductItem1.setUpcCode(ProductItemServiceTest.UPC_CODE_1);
    List<ProductItemImage> productItemImages = new ArrayList<ProductItemImage>();
    List<ProductItemImage> secondItemImages = new ArrayList<ProductItemImage>();
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setId("1234");
    productItemImage.setMarkForDelete(false);
    productItemImages.add(productItemImage);
    secondItemImages.add(productItemImage);
    ProductItemImage secondProductImage = new ProductItemImage();
    secondProductImage.setId("12345");
    secondProductImage.setMarkForDelete(true);
    productItemImages.add(secondProductImage);
    secondItemImages.add(secondProductImage);
    ProductItemImage thirdProductImage = new ProductItemImage();
    thirdProductImage.setId("123456");
    thirdProductImage.setMarkForDelete(true);
    secondItemImages.add(thirdProductImage);
    savedProductItem1.setProductItemImages(secondItemImages);
    Product parentProduct = new Product();
    parentProduct.setId(productId);
    parentProduct.setProductCode(productCode);
    savedProductItem1.setProduct(parentProduct);
    savedProductItem1.setProductId(productId);
    savedProductItem1.setSkuCode("MTA-23456-00001");
    List<ProductItem> savedProductItems = new ArrayList<>();
    savedProductItems.add(savedProductItem1);


    ProductItem savedProductItem2 = new ProductItem();
    savedProductItem2.setId("2");
    savedProductItem2.setUpcCode(ProductItemServiceTest.UPC_CODE_1);
    productItemImages = new ArrayList<ProductItemImage>();
    secondItemImages = new ArrayList<ProductItemImage>();
    productItemImage = new ProductItemImage();
    productItemImage.setId("1234");
    productItemImage.setMarkForDelete(false);
    productItemImages.add(productItemImage);
    secondItemImages.add(productItemImage);
    secondProductImage = new ProductItemImage();
    secondProductImage.setId("12345");
    secondProductImage.setMarkForDelete(true);
    productItemImages.add(secondProductImage);
    secondItemImages.add(secondProductImage);
    thirdProductImage = new ProductItemImage();
    thirdProductImage.setId("123456");
    thirdProductImage.setMarkForDelete(true);
    secondItemImages.add(thirdProductImage);
    savedProductItem2.setProductItemImages(secondItemImages);
    parentProduct = new Product();
    parentProduct.setId(productId);
    parentProduct.setProductCode(productCode);
    savedProductItem2.setProduct(parentProduct);
    savedProductItem2.setProductId(productId);
    savedProductItem2.setSkuCode("MTA-23456-00001");
    savedProductItems.add(savedProductItem2);


    ProductItem savedProductItem3 = new ProductItem();
    savedProductItem3.setId("3");
    savedProductItem3.setUpcCode(ProductItemServiceTest.UPC_CODE_1);
    productItemImages = new ArrayList<ProductItemImage>();
    secondItemImages = new ArrayList<ProductItemImage>();
    productItemImage = new ProductItemImage();
    productItemImage.setId("1234");
    productItemImage.setMarkForDelete(false);
    productItemImages.add(productItemImage);
    secondItemImages.add(productItemImage);
    secondProductImage = new ProductItemImage();
    secondProductImage.setId("12345");
    secondProductImage.setMarkForDelete(true);
    productItemImages.add(secondProductImage);
    secondItemImages.add(secondProductImage);
    thirdProductImage = new ProductItemImage();
    thirdProductImage.setId("123456");
    thirdProductImage.setMarkForDelete(true);
    secondItemImages.add(thirdProductImage);
    savedProductItem3.setProductItemImages(secondItemImages);
    savedProductItem3.setSkuCode("MTA-12345-00001");




    List<ProductItem> productItems = new ArrayList<ProductItem>();
    ProductItem productItem = new ProductItem();
    productItem.setId("2");
    productItem.setSkuCode("MTA-234567-00001");
    productItem.setUpcCode(ProductItemServiceTest.UPC_CODE_1);
    productItem.setProduct(parentProduct);
    productItem.setProductItemImages(productItemImages);
    productItems.add(productItem);

    productItem = new ProductItem();
    productItem.setId("3");
    productItem.setSkuCode("MTA-12345-00001");
    productItem.setUpcCode(ProductItemServiceTest.UPC_CODE_1);
    productItem.setProduct(parentProduct);
    productItem.setProductItemImages(productItemImages);
    productItems.add(productItem);

    parentProduct.setProductItems(savedProductItems);
    parentProduct.getProductItems().add(savedProductItem3);
    savedProductItem3.setProduct(parentProduct);
    savedProductItem3.setProductId(productId);
    savedProductItems.add(savedProductItem3);
    when(this.repository.findById(productItem.getId())).thenReturn(Optional.of(savedProductItem3));
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(eq(STORE_ID), anyString()))
        .thenReturn(savedProductItem3);
    when(this.productRepository.findById(productId)).thenReturn(Optional.of(parentProduct));
    when(productService.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId))
        .thenReturn(parentProduct);

    this.service.saveProductItemDetails(ProductItemServiceTest.STORE_ID, productItems, "MTA-12345");
    verify(this.productRepository, times(2)).findById(productId);
    verify(this.productRepository, times(2)).saveAndFlush(parentProduct);
    verify(this.repository, times(2)).findByStoreIdAndIdAndMarkForDeleteFalse(eq(STORE_ID), anyString());
    verify(repository, times(4)).getSequenceByProductCode(anyString()) ;
  }
 
  @Test
  public void saveProductItemDetailWithNullIdTest() throws Exception {
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    productItems.add(new ProductItem());
    try {
      this.service.saveProductItemDetails(ProductItemServiceTest.STORE_ID, productItems, "MTA-12345");
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationException);
      assertTrue(e.getMessage().contains(ErrorCategory.DATA_NOT_FOUND.getMessage()));
      verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID, null);
    }
  }

  @Test
  public void saveProductItemTest() throws Exception {
    ProductItem savedProductItem = new ProductItem();
    BeanUtils.copyProperties(this.productItem, savedProductItem);
    when(this.repository.findById(this.productItem.getId())).thenReturn(Optional.ofNullable(null));
    when(this.repository.saveAndFlush(this.productItem)).thenReturn(savedProductItem);
    assertEquals(this.service.save(this.productItem), (savedProductItem.getId()));

    verify(this.repository, times(1)).saveAndFlush(this.productItem);
    verify(this.repository, times(1)).findById(this.productItem.getId());
  }

  @Test
  public void saveProductItemWithExistingIdTest() throws Exception {
    ProductItem savedProductItem = new ProductItem();
    BeanUtils.copyProperties(this.productItem, savedProductItem);

    when(this.repository.findById(this.productItem.getId())).thenReturn(Optional.of(savedProductItem));
    try {
      this.service.save(this.productItem);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ErrorCategory.DATA_ACCESS.getMessage())) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
      verify(this.repository, times(1)).findById(this.productItem.getId());
    }
  }

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    this.product = new Product();
    product.setId(productId);
    productItem = new ProductItem();
    productItem.setId(PRODUCT_ITEM_ID_1);
    productItem.setUpcCode(UPC_CODE);
    productItem.setSkuCode(SKU_CODE);
    productItem.setStoreId(STORE_ID);
    productItem.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItem.setHash(HASH);
    this.productItem.setProductId(productId);
    this.productItem1 = new ProductItem();
    productItem1.setId(PRODUCT_ITEM_ID_2);
    productItem1.setUpcCode(UPC_CODE_1);
    productItem1.setHash(HASH_1);
    productItem1.setSkuCode(SKU_CODE_1);
    productItem1.setGeneratedItemName(GENERATED_ITEM_NAME_1);
    productItem1.setStoreId(STORE_ID);
    productItem1.setProductId(productId);

    this.productItemActive = new ProductItem();
    productItemActive.setId(PRODUCT_ITEM_ID_2);
    productItemActive.setUpcCode(UPC_CODE);
    productItemActive.setSkuCode(SKU_CODE);
    productItemActive.setStoreId(STORE_ID);
    productItemActive.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemActive.setHash(HASH);
    this.productItemActive.setMarkForDelete(true);

    this.emptyProductItems = new ArrayList<>();
    this.productItems = new ArrayList<>();
    this.productItems.add(this.productItem);
    this.productItemsPage1 =
        new PageImpl<>(this.productItems, ProductItemServiceTest.DEFAULT_PAGEABLE, this.productItems.size());

    this.productItems.add(this.productItem1);
    this.productItemsPage =
        new PageImpl<>(this.productItems, ProductItemServiceTest.DEFAULT_PAGEABLE, this.productItems.size());

    this.skuCodes = new ArrayList<String>();
    this.skuCodes.add("sku");

    this.skuCodesRegex = "(sku)";

    productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(NEW_LOCATION_PATH);
    productItemImage.setActive(true);

    Attribute attribute1 = new Attribute();
    attribute1.setId(ATTRIBUTE_ID_1);
    attribute1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute1.setAttributeCode(ATTRIBUTE_CODE_1);
    attribute1.setName(ATTRIBUTE_NAME_1);

    Attribute attribute2 = new Attribute();
    attribute2.setId(ATTRIBUTE_ID_2);
    attribute2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attribute2.setAttributeCode(ATTRIBUTE_CODE_2);
    attribute2.setName(ATTRIBUTE_NAME_2);

    Attribute attribute3 = new Attribute();
    attribute3.setId(ATTRIBUTE_ID_3);
    attribute3.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute3.setAttributeCode(ATTRIBUTE_CODE_3);
    attribute3.setName(ATTRIBUTE_NAME_3);

    productItemAttributeValue1 = new ProductItemAttributeValue();
    productItemAttributeValue1.setAttributeId(ATTRIBUTE_ID_1);
    productItemAttributeValue1.setAttribute(attribute1);
    productItemAttributeValue1.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_1);

    productItemAttributeValue2 = new ProductItemAttributeValue();
    productItemAttributeValue2.setAttributeId(ATTRIBUTE_ID_2);
    productItemAttributeValue2.setAttribute(attribute2);
    productItemAttributeValue2.setMarkForDelete(true);
    productItemAttributeValue2.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_2);

    productItemAttributeValue3 = new ProductItemAttributeValue();
    productItemAttributeValue3.setAttributeId(ATTRIBUTE_ID_3);
    productItemAttributeValue3.setAttribute(attribute3);
    productItemAttributeValue3.setValue(PRODUCT_ITEM_ATTRIBUTE_VALUE_3);

    productItemImage1 = new ProductItemImage();
    productItemImage1.setId(IMAGE_ID_1);
    productItemImage1.setOriginalImage(Boolean.TRUE);
    productItemImage1.setMainImages(true);

    productItemImage2 = new ProductItemImage();
    productItemImage2.setId(IMAGE_ID_2);
    productItemImage2.setMarkForDelete(true);
    productItemImage2.setOriginalImage(Boolean.FALSE);
    productItemImage2.setMainImages(true);

    productItemImage3 = new ProductItemImage();
    productItemImage3.setId(IMAGE_ID_3);
    productItemImage3.setMarkForDelete(false);
    productItemImage3.setOriginalImage(Boolean.FALSE);
    productItemImage3.setActive(true);

    when(repository.findByStoreIdAndProductId(STORE_ID, productId)).thenReturn(productItems);
    when(imageService.getDetachedProductItemImagesByStoreIdAndProductItemIds(
        STORE_ID, Collections.singletonList(PRODUCT_ITEM_ID_1)))
        .thenReturn(new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2, productItemImage3)));
    when(productItemAttributeValueService
        .getDetachedProductItemAttributeValuesByStoreIdAndProductItemIds(STORE_ID, Collections.singletonList(PRODUCT_ITEM_ID_1)))
        .thenReturn(new ArrayList<>(Arrays.asList(productItemAttributeValue1, productItemAttributeValue2,
            productItemAttributeValue3)));
    when(attributeService.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(
        STORE_ID, ATTRIBUTE_ID_1)).thenReturn(attribute1);
    when(attributeService.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(
        STORE_ID, ATTRIBUTE_ID_1)).thenReturn(attribute2);
    when(attributeService.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(
        STORE_ID, ATTRIBUTE_ID_1)).thenReturn(attribute3);

    when(applicationContext.getBean(ProductItemService.class)).thenReturn(service);

    when(this.repository.countByStoreIdAndSkuCode(ProductItemServiceTest.STORE_ID, ProductItemServiceTest.SKU_CODE))
    .thenReturn(1L);
    when(
        this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
            this.productItem.getId())).thenReturn(this.productItem);
    when(
        this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
            this.productItem1.getId())).thenReturn(this.productItem1);
    when(
        this.repository.findByStoreIdAndUpcCodeMatches(ProductItemServiceTest.STORE_ID, this.skuCodesRegex,
            ProductItemServiceTest.DEFAULT_PAGEABLE)).thenReturn(this.productItemsPage);
    when(
        this.repository.findByStoreIdAndUpcCodeExcludeOneItemMatches(ProductItemServiceTest.STORE_ID,
            this.skuCodesRegex, ProductItemServiceTest.SKU_CODE_1, ProductItemServiceTest.DEFAULT_PAGEABLE))
            .thenReturn(this.productItemsPage1);
    when(
        this.repository.findByStoreIdAndSkuCodeAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
            ProductItemServiceTest.SKU_CODE)).thenReturn(this.productItem);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.repository);
    verifyNoMoreInteractions(this.solrProductDao);
    verifyNoMoreInteractions(this.imageService);
    verifyNoMoreInteractions(attributeService);
    verifyNoMoreInteractions(applicationContext);
    verifyNoMoreInteractions(productItemAttributeValueService);
    verifyNoMoreInteractions(productService);
    verifyNoMoreInteractions(this.categoryService);
  }

  @Test
  public void testFindBySkuCodes() throws Exception {
    List<String> skuCodes = new ArrayList<String>();
    productItem.setProductItemAttributeValues(Arrays.asList(productItemAttributeValue1, productItemAttributeValue2));
    skuCodes.add(this.productItem.getSkuCode());
    skuCodes.add(this.productItem1.getSkuCode());
    when(this.repository.findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(STORE_ID, skuCodes)).thenReturn(productItems);
    List<ProductItem> result = this.service.findBySkuCodes(STORE_ID, skuCodes, false);
    verify(this.repository).findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(STORE_ID, skuCodes);
    verify(productService, times(2))
        .getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
    assertEquals(2, result.size());
  }

  @Test
  public void testFindBySkuCodes_whenFetchArchivedTrue() throws Exception {
    List<String> skuCodes = new ArrayList<String>();
    skuCodes.add(this.productItem.getSkuCode());
    skuCodes.add(this.productItem1.getSkuCode());
    when(this.repository.findByStoreIdAndSkuCodeIn(STORE_ID, skuCodes)).thenReturn(productItems);
    List<ProductItem> result = this.service.findBySkuCodes(STORE_ID, skuCodes, true);
    verify(this.repository).findByStoreIdAndSkuCodeIn(STORE_ID, skuCodes);
    assertEquals(2, result.size());
    assertEquals(SKU_CODE, result.get(0).getSkuCode());
    assertEquals(SKU_CODE_1, result.get(1).getSkuCode());
  }

	@Test
	public void testFindProductItemByProductIdInitAttrValue() throws Exception {
		this.service.findProductItemByProductIdInitAttrValue(ProductItemServiceTest.STORE_ID, this.product.getId());
		verify(repository).findByStoreIdAndProductId(STORE_ID, productId);
		verify(applicationContext).getBean(ProductItemService.class);
		verify(productService, times(2))
        .getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
	}

  @Test
  public void testIsUpcAvailable() throws Exception {
    when(
        this.repository.findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
            ProductItemServiceTest.UPC_CODE, true)).thenReturn(this.emptyProductItems);
    this.service.isUpcAvailable(ProductItemServiceTest.STORE_ID, ProductItemServiceTest.UPC_CODE, true);
    verify(this.repository, times(1)).findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(
        ProductItemServiceTest.STORE_ID, ProductItemServiceTest.UPC_CODE, true);
  }
  
	@Test
	public void IsUpcAvailableTest() throws Exception {
		List<ProductItem> productItems = new ArrayList<ProductItem>();
		productItems.add(this.productItem);
		this.emptyProductItems = productItems;
		when(this.repository.findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(ProductItemServiceTest.STORE_ID,
				ProductItemServiceTest.UPC_CODE, true)).thenReturn(this.emptyProductItems);
		this.service.isUpcAvailable(ProductItemServiceTest.STORE_ID, ProductItemServiceTest.UPC_CODE, true);
		verify(this.repository, times(1)).findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(
				ProductItemServiceTest.STORE_ID, ProductItemServiceTest.UPC_CODE, true);
	}

  @Test
  public void updateProductItemTest() throws Exception {
    ProductItem savedProductItem = new ProductItem();
    BeanUtils.copyProperties(this.productItem, savedProductItem);
    when(this.repository.findById(this.productItem.getId())).thenReturn(Optional.of(savedProductItem));
    this.service.update(this.productItem);
    assertTrue(true);
    verify(this.repository, times(1)).findById(this.productItem.getId());
    verify(this.repository, times(1)).saveAndFlush(this.productItem);
  }

  @Test
  public void updateProductItemWithNonExistenceIdTest() throws Exception {
    when(this.repository.findById(this.productItem.getId())).thenReturn(Optional.ofNullable(null));
    try {
      this.service.update(this.productItem);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ErrorCategory.DATA_NOT_FOUND.getMessage())) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
    }
    verify(this.repository).findById(this.productItem.getId());
  }

  @Test
  public void updateProuctItemWithNullIdTest() throws Exception {
    try {
      this.service.update(new ProductItem());
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ErrorCategory.DATA_NOT_FOUND.getMessage())) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
    }
  }

  @Test
  public void findByStoreIdAndGeneratedItemNameAndCategoryIdTest() throws Exception {
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    productItems.add(this.productItem);
    productItems.add(this.productItem1);
    when(solrProductDao
        .findProductItemsWithCategoryIdAndGeneratedItemName(GENERATED_ITEM_NAME_1, "", solrStart,
            solrRows)).thenReturn(productItems);
    this.service.findByStoreIdAndGeneratedItemNameAndCategoryId(STORE_ID, GENERATED_ITEM_NAME_1, "",
        DEFAULT_PAGEABLE);
    verify(solrProductDao)
        .findProductItemsWithCategoryIdAndGeneratedItemName(GENERATED_ITEM_NAME_1, "", solrStart,
            solrRows);
  }

  @Test
  public void findByListOfProductCodeTest() throws Exception {
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    productItems.add(this.productItem);
    productItems.add(this.productItem1);
    when(this.repository.findByListProductCodeWithActivatedAndViewableTrueAndPromoSkuFalse(Mockito.anyList()))
        .thenReturn(productItems);
    this.service
        .findByListOfProductCode(new ArrayList<String>(), true, true);
    verify(this.repository)
        .findByListProductCodeWithActivatedAndViewableTrueAndPromoSkuFalse(Mockito.anyList());
    verify(productService, times(2))
        .getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
  }

  @Test
  public void findByListOfProductCodeTest_2() throws Exception {
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    productItems.add(this.productItem);
    productItems.add(this.productItem1);
    when(this.repository.findByListProductCodeWithActivatedTrueAndViewableTrue(Mockito.anyList()))
        .thenReturn(productItems);
    this.service
        .findByListOfProductCode(new ArrayList<String>(), false, true);
    verify(this.repository)
        .findByListProductCodeWithActivatedTrueAndViewableTrue(Mockito.anyList());
    verify(productService, times(2))
        .getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
  }

  @Test
  public void findByListOfProductCodeActiveFalseTest() throws Exception {
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    productItems.add(this.productItem);
    productItems.add(this.productItem1);
    when(this.repository.findByListProductCodeWithPromoSkuFalseAndMarkForDeleteFalse(
        Collections.singletonList(PRODUCT_CODE))).thenReturn(productItems);
    this.service.findByListOfProductCode(Collections.singletonList(PRODUCT_CODE), true, false);
    verify(this.repository).findByListProductCodeWithPromoSkuFalseAndMarkForDeleteFalse(
        Collections.singletonList(PRODUCT_CODE));
    verify(productService, times(2)).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
  }

  @Test
  public void findByListOfProductCodeActiveFalseTest_2() throws Exception {
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    productItems.add(this.productItem);
    productItems.add(this.productItem1);
    when(this.repository.findByListProductCodeWithMarkForDeleteFalse(
        Collections.singletonList(PRODUCT_CODE))).thenReturn(productItems);
    this.service.findByListOfProductCode(Collections.singletonList(PRODUCT_CODE), false, false);
    verify(this.repository).findByListProductCodeWithMarkForDeleteFalse(Collections.singletonList(PRODUCT_CODE));
    verify(productService, times(2)).getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
  }

  @Test
  public void findListOfItemsByProductTest() throws Exception {
    Mockito.when(this.repository
        .findByStoreIdAndProductAndActivatedTrueAndViewableTrueAndMarkForDeleteFalseOrderBySkuCode(
            STORE_ID, product, DEFAULT_PAGEABLE))
        .thenReturn(new PageImpl(Arrays.asList(productItem), DEFAULT_PAGEABLE, TOTAL_RECORDS));
    Page<ProductItem> productItemPage = this.service.findListOfItemsByProduct(STORE_ID, product, DEFAULT_PAGEABLE);
    Mockito.verify(this.repository)
        .findByStoreIdAndProductAndActivatedTrueAndViewableTrueAndMarkForDeleteFalseOrderBySkuCode(
            STORE_ID, product, DEFAULT_PAGEABLE);
    Assertions.assertEquals(SKU_CODE, productItemPage.getContent().get(0).getSkuCode());
    Assertions.assertEquals(GENERATED_ITEM_NAME, productItemPage.getContent().get(0).getGeneratedItemName());
    Assertions.assertEquals(STORE_ID, productItemPage.getContent().get(0).getStoreId());
    Assertions.assertEquals(product, productItemPage.getContent().get(0).getProduct());
  }

  @Test
  public void getCategoryIdsWithProductCountForUPCCode_IsOnlyExternalFlagTrue() {
    Object[] objects = {CATEGORY_ID, 1L};
    List<Object[]> list = new ArrayList<>();
    list.add(objects);
    when(this.repository
        .findProductIdByUPCCodeWithActivatedAndViewableTrueAndReviewPendingFalseAndPromoSkuFalse(UPC_CODE))
        .thenReturn(Arrays.asList(productId));
    when(this.productCategoryService.findCategoryCountByProductIdInAndMarkForDeleteFalse(Arrays.asList(productId)))
        .thenReturn(list);
    Map<String, Long> response = this.service.getCategoryIdsWithProductCountForUPCCode(UPC_CODE, true);
    verify(this.repository)
        .findProductIdByUPCCodeWithActivatedAndViewableTrueAndReviewPendingFalseAndPromoSkuFalse(UPC_CODE);
    verify(this.productCategoryService)
        .findCategoryCountByProductIdInAndMarkForDeleteFalse(Arrays.asList(productId));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.size());
    Assertions.assertEquals(1, response.get(CATEGORY_ID).longValue());
  }

  @Test
  public void getCategoryIdsWithProductCountForUPCCode_IsOnlyExternalFlagFalse() {
    Object[] objects = {CATEGORY_ID, 1L};
    List<Object[]> list = new ArrayList<>();
    list.add(objects);
    when(this.repository.findProductIdByUPCCodeWithActivatedTrueAndViewableTrueAndReviewPendingFalse(UPC_CODE))
        .thenReturn(Arrays.asList(productId));
    when(this.productCategoryService.findCategoryCountByProductIdInAndMarkForDeleteFalse(Arrays.asList(productId)))
        .thenReturn(list);
    Map<String, Long> response = this.service.getCategoryIdsWithProductCountForUPCCode(UPC_CODE, false);
    verify(this.repository).findProductIdByUPCCodeWithActivatedTrueAndViewableTrueAndReviewPendingFalse(UPC_CODE);
    verify(this.productCategoryService)
        .findCategoryCountByProductIdInAndMarkForDeleteFalse(Arrays.asList(productId));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.size());
    Assertions.assertEquals(1, response.get(CATEGORY_ID).longValue());
  }

  @Test
  public void getCategoryIdsWithProductCountForUPCCode_emptyListReturned() {
    List<Object[]> list = new ArrayList<>();
    when(this.repository
        .findProductIdByUPCCodeWithActivatedAndViewableTrueAndReviewPendingFalseAndPromoSkuFalse(UPC_CODE))
        .thenReturn(Arrays.asList(productId));
    when(this.productCategoryService.findCategoryCountByProductIdInAndMarkForDeleteFalse(Arrays.asList(productId)))
        .thenReturn(list);
    Map<String, Long> response = this.service.getCategoryIdsWithProductCountForUPCCode(UPC_CODE, true);
    verify(this.repository)
        .findProductIdByUPCCodeWithActivatedAndViewableTrueAndReviewPendingFalseAndPromoSkuFalse(UPC_CODE);
    verify(this.productCategoryService)
        .findCategoryCountByProductIdInAndMarkForDeleteFalse(Arrays.asList(productId));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(0, response.size());
  }

  @Test
  public void getCategoryIdsWithProductCountForUPCCodeExceptionTest() {
    when(this.repository
        .findProductIdByUPCCodeWithActivatedAndViewableTrueAndReviewPendingFalseAndPromoSkuFalse(UPC_CODE))
        .thenReturn(Arrays.asList(productId));
    when(this.productCategoryService.findCategoryCountByProductIdInAndMarkForDeleteFalse(Arrays.asList(productId)))
        .thenThrow(new ApplicationRuntimeException());
    try {
      Map<String, Long> response = this.service.getCategoryIdsWithProductCountForUPCCode(UPC_CODE, true);
    } catch (ApplicationRuntimeException e) {
      verify(this.repository)
          .findProductIdByUPCCodeWithActivatedAndViewableTrueAndReviewPendingFalseAndPromoSkuFalse(UPC_CODE);
      verify(this.productCategoryService)
          .findCategoryCountByProductIdInAndMarkForDeleteFalse(Arrays.asList(productId));
    }
  }

  @Test
  public void getItemDetailsByItemCodeTest() throws Exception {
    Mockito.when(this.repository.findByStoreIdAndSkuCodeAndMarkForDeleteFalse(STORE_ID, SKU_CODE))
        .thenReturn(productItem);
    ProductItem result =
        this.service.getProductItemByItemCode(STORE_ID, SKU_CODE);
    Mockito.verify(this.repository).findByStoreIdAndSkuCodeAndMarkForDeleteFalse(STORE_ID, SKU_CODE);
    Assertions.assertEquals(UPC_CODE, productItem.getUpcCode());
    Assertions.assertEquals(SKU_CODE, productItem.getSkuCode());
    Assertions.assertEquals(GENERATED_ITEM_NAME, productItem.getGeneratedItemName());
  }


  @Test
  public void getItemDetailsByItemCode_expectException() throws Exception {
    Mockito.when(this.repository.findByStoreIdAndSkuCodeAndMarkForDeleteFalse(STORE_ID, SKU_CODE))
        .thenReturn(null);
    try {
      ProductItem result =
          this.service.getProductItemByItemCode(STORE_ID, SKU_CODE);
    } catch (Exception e) {
    } finally {
      Mockito.verify(this.repository).findByStoreIdAndSkuCodeAndMarkForDeleteFalse(STORE_ID, SKU_CODE);
    }
  }

  @Test
  public void getItemsByStoreIdAndProductIdTest() {
    Mockito.when(repository.findByStoreIdAndProductId(STORE_ID, productId)).thenReturn(productItems);
    List<ProductItem> response = service.getProductItemsByStoreIdAndProductId(STORE_ID, productId);
    Mockito.verify(repository).findByStoreIdAndProductId(STORE_ID, productId);
    Assertions.assertEquals(response, productItems);
  }

  @Test
  public void findByStoreIdAndGeneratedItemNameTest() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    when(repository.findByStoreIdAndGeneratedItemNameContainingIgnoreCaseAndMarkForDeleteFalse(
        STORE_ID, GENERATED_ITEM_NAME, pageable)).thenReturn(new PageImpl<>(productItems));
    service.findByStoreIdAndGeneratedItemName(STORE_ID, GENERATED_ITEM_NAME, pageable);
    verify(repository).findByStoreIdAndGeneratedItemNameContainingIgnoreCaseAndMarkForDeleteFalse(
            STORE_ID, GENERATED_ITEM_NAME, pageable);
    verify(productService, times(2))
        .getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
  }

  @Test
  public void findByUpcCodeTest() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    when(repository.findByStoreIdAndUpcCodeStartingWithIgnoreCaseAndMarkForDeleteFalse(
        STORE_ID, UPC_CODE, pageable)).thenReturn(new PageImpl<>(productItems));
    service.findByUpcCode(STORE_ID, UPC_CODE, pageable);
    verify(repository).findByStoreIdAndUpcCodeStartingWithIgnoreCaseAndMarkForDeleteFalse(
        STORE_ID, UPC_CODE, pageable);
    verify(productService, times(2))
        .getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
  }

  @Test
  public void getProductItemsByUpcCodeIsOnlyExternalTrueTest() {
    List<String> productItemIds = Arrays.asList(PRODUCT_ITEM_ID_1, PRODUCT_ITEM_ID_2);
    when(repository.findByUPCCodeWithActivatedAndViewableTrueAndReviewPendingFalseAndPromoSkuFalse(
        UPC_CODE, Collections.singletonList(CATEGORY_ID)))
        .thenReturn(productItemIds);
    Assertions.assertEquals(productItemIds, service.getProductItemsIdsByUpcCodeAndCategoryIds(
        UPC_CODE, true, Collections.singletonList(CATEGORY_ID)));
    verify(repository)
        .findByUPCCodeWithActivatedAndViewableTrueAndReviewPendingFalseAndPromoSkuFalse(
            UPC_CODE, Collections.singletonList(CATEGORY_ID));
  }

  @Test
  public void getProductItemsByUpcCodeIsOnlyExternalFalseTest() {
    List<String> productItemIds = Arrays.asList(PRODUCT_ITEM_ID_1, PRODUCT_ITEM_ID_2);
    when(repository.findByUPCCodeWithActivatedTrueAndViewableTrueAndReviewPendingFalse(
        UPC_CODE, Collections.singletonList(CATEGORY_ID)))
        .thenReturn(productItemIds);
    Assertions.assertEquals(productItemIds, service.getProductItemsIdsByUpcCodeAndCategoryIds(
        UPC_CODE, false, Collections.singletonList(CATEGORY_ID)));
    verify(repository)
        .findByUPCCodeWithActivatedTrueAndViewableTrueAndReviewPendingFalse(
            UPC_CODE, Collections.singletonList(CATEGORY_ID));
  }

  @Test
  public void findByUpcCodeExactMatchTest(){
    Pageable pageable = PageRequest.of(0, 10);
    when(repository.findByStoreIdAndUpcCodeIgnoreCaseAndMarkForDeleteFalse(
        STORE_ID, UPC_CODE, pageable)).thenReturn(new PageImpl<>(productItems));
    service.findByUpcCodeExactMatch(STORE_ID, UPC_CODE, pageable);
    verify(repository).findByStoreIdAndUpcCodeIgnoreCaseAndMarkForDeleteFalse(
        STORE_ID, UPC_CODE, pageable);
    verify(productService, times(2))
        .getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(STORE_ID, productId);
  }

  @Test
  public void removeDeletedProductItemImagesWithoutFilteringMainImagesTest() {
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2, productItemImage3)));
    service.removeDeletedProductItemImagesWithoutFilteringMainImages(productItem);
    Assertions.assertEquals(2, productItem.getProductItemImages().size());
  }

  @Test
  public void testRemoveDeletedAndSetMainImageFlagForProductItemImages_NoImages() {
    ProductItemDTO productItemDTO = new ProductItemDTO();
    service.removeDeletedAndSetMainImageFlagForProductItemImages(productItemDTO);
    assertTrue(productItemDTO.getProductItemImageDTOS().isEmpty());
  }

  @Test
  public void testRemoveDeletedAndSetMainImageFlagForProductItemImages_RemoveDeleted() {
    ProductItemDTO productItemDTO = new ProductItemDTO();
    List<ProductItemImageDTO> productItemImageDTOS = new ArrayList<>();
    ProductItemImageDTO image1 = new ProductItemImageDTO();
    image1.setMarkForDelete(true);
    productItemImageDTOS.add(image1);
    ProductItemImageDTO image2 = new ProductItemImageDTO();
    image2.setMarkForDelete(false);
    productItemImageDTOS.add(image2);
    productItemDTO.setProductItemImageDTOS(productItemImageDTOS);
    service.removeDeletedAndSetMainImageFlagForProductItemImages(productItemDTO);
    assertEquals(1, productItemDTO.getProductItemImageDTOS().size());
    assertTrue(productItemDTO.getProductItemImageDTOS().contains(image2));
  }

  @Test
  public void testRemoveDeletedAndSetMainImageFlagForProductItemImages_SetMainImageFlags() {
    ProductItemDTO productItemDTO = new ProductItemDTO();
    List<ProductItemImageDTO> productItemImageDTOS = new ArrayList<>();
    ProductItemImageDTO image1 = new ProductItemImageDTO();
    image1.setMainImages(true);
    image1.setOriginalImage(true);
    productItemImageDTOS.add(image1);
    ProductItemImageDTO image2 = new ProductItemImageDTO();
    image2.setMainImages(true);
    productItemImageDTOS.add(image2);
    productItemDTO.setProductItemImageDTOS(productItemImageDTOS);
    service.removeDeletedAndSetMainImageFlagForProductItemImages(productItemDTO);
    assertEquals(2, productItemDTO.getProductItemImageDTOS().size());
    assertTrue(productItemDTO.getProductItemImageDTOS().contains(image1));
    assertTrue(productItemDTO.getProductItemImageDTOS().contains(image2));
    assertTrue(image1.isMainImages());
  }

  @Test
  public void testRemoveDeletedAndSetMainImageFlagForProductItemImages_SetMainImageFlagOriginalImageFalse() {
    ProductItemDTO productItemDTO = new ProductItemDTO();
    List<ProductItemImageDTO> productItemImageDTOS = new ArrayList<>();
    ProductItemImageDTO image1 = new ProductItemImageDTO();
    image1.setMainImages(true);
    image1.setOriginalImage(false);
    productItemImageDTOS.add(image1);
    ProductItemImageDTO image2 = new ProductItemImageDTO();
    image2.setMainImages(false);
    image1.setOriginalImage(false);

    productItemImageDTOS.add(image2);
    productItemDTO.setProductItemImageDTOS(productItemImageDTOS);
    service.removeDeletedAndSetMainImageFlagForProductItemImages(productItemDTO);
    assertEquals(2, productItemDTO.getProductItemImageDTOS().size());
    assertTrue(productItemDTO.getProductItemImageDTOS().contains(image1));
    assertTrue(productItemDTO.getProductItemImageDTOS().contains(image2));
    assertTrue(image1.isMainImages());
  }

  @Test
  public void testRemoveDeletedAndSetMainImageFlagForProductItemImages_SetMainImageFlagOriginalImage() {
    ProductItemDTO productItemDTO = new ProductItemDTO();
    List<ProductItemImageDTO> productItemImageDTOS = new ArrayList<>();
    ProductItemImageDTO image1 = new ProductItemImageDTO();
    image1.setMainImages(true);
    image1.setOriginalImage(true);
    productItemImageDTOS.add(image1);
    ProductItemImageDTO image2 = new ProductItemImageDTO();
    image2.setMainImages(false);
    image1.setOriginalImage(true);
    productItemImageDTOS.add(image2);
    productItemDTO.setProductItemImageDTOS(productItemImageDTOS);
    service.removeDeletedAndSetMainImageFlagForProductItemImages(productItemDTO);
    assertEquals(2, productItemDTO.getProductItemImageDTOS().size());
    assertTrue(productItemDTO.getProductItemImageDTOS().contains(image1));
    assertTrue(productItemDTO.getProductItemImageDTOS().contains(image2));
    assertTrue(image1.isMainImages());
  }

  @Test
  public void testRemoveDeletedAndSetMainImageFlagForProductItemImages_NoDeletedNoMain() {
    ProductItemDTO productItemDTO = new ProductItemDTO();
    List<ProductItemImageDTO> productItemImageDTOS = new ArrayList<>();
    ProductItemImageDTO image1 = new ProductItemImageDTO();
    image1.setSequence(0);
    productItemImageDTOS.add(image1);
    ProductItemImageDTO image2 = new ProductItemImageDTO();
    image2.setSequence(1);
    productItemImageDTOS.add(image2);
    productItemDTO.setProductItemImageDTOS(productItemImageDTOS);
    service.removeDeletedAndSetMainImageFlagForProductItemImages(productItemDTO);
    assertEquals(2, productItemDTO.getProductItemImageDTOS().size());
    assertTrue(productItemDTO.getProductItemImageDTOS().contains(image1));
    assertTrue(productItemDTO.getProductItemImageDTOS().contains(image2));
  }


  @Test
  public void removeDeletedProductItemImagesWithoutFilteringMainImagesNullOrignalImageTest() {
    productItemImage1.setOriginalImage(null);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setMainImages(true);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2, productItemImage3)));
    service.removeDeletedProductItemImagesWithoutFilteringMainImages(productItem);
    Assertions.assertEquals(2, productItem.getProductItemImages().size());
  }

  @Test
  public void removeDeletedProductItemImagesWithMultipleMainImagesWithoutFilteringMainImagesTest() {
    productItemImage2.setMainImages(true);
    productItemImage2.setMarkForDelete(false);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2)));
    service.removeDeletedProductItemImagesWithoutFilteringMainImages(productItem);
    Assertions.assertEquals(2, productItem.getProductItemImages().size());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).isMainImages());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).getOriginalImage());
    Assertions.assertTrue(productItem.getProductItemImages().get(1).isMainImages());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).getOriginalImage());
  }

  @Test
  public void removeDeletedProductItemImagesWithMultipleMainAndOriginalImagesWithoutFilteringMainImagesTest() {
    productItemImage1.setOriginalImage(true);
    productItemImage2.setMainImages(true);
    productItemImage2.setOriginalImage(true);
    productItemImage2.setMarkForDelete(false);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2)));
    service.removeDeletedProductItemImagesWithoutFilteringMainImages(productItem);
    Assertions.assertEquals(2, productItem.getProductItemImages().size());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).isMainImages());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).getOriginalImage());
  }

  @Test
  public void removeDeletedProductItemImagesWithNoMainImageWithoutFilteringMainImagesTest() {
    productItemImage1.setMainImages(false);
    productItemImage1.setSequence(0);
    productItemImage2.setMainImages(false);
    productItemImage2.setSequence(1);
    productItemImage2.setMarkForDelete(false);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2)));
    service.removeDeletedProductItemImagesWithoutFilteringMainImages(productItem);
    Assertions.assertEquals(2, productItem.getProductItemImages().size());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).isMainImages());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).getOriginalImage());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).isMainImages());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).getOriginalImage());
  }

  @Test
  public void removeDeletedProductItemImagesWithNoMainImageWithoutFilteringMainImagesNullSequenceTest() {
    productItemImage1.setMainImages(false);
    productItemImage1.setSequence(null);
    productItemImage2.setMainImages(false);
    productItemImage2.setSequence(null);
    productItemImage2.setMarkForDelete(false);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2)));
    service.removeDeletedProductItemImagesWithoutFilteringMainImages(productItem);
    Assertions.assertEquals(2, productItem.getProductItemImages().size());
  }

  @Test
  public void removeDeletedProductItemImagesWithMultipleMainAndNonOriginalImagesWithoutFilteringMainImgaesTest() {
    productItemImage1.setOriginalImage(false);
    productItemImage2.setMainImages(true);
    productItemImage2.setOriginalImage(false);
    productItemImage2.setMarkForDelete(false);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2)));
    service.removeDeletedProductItemImagesWithoutFilteringMainImages(productItem);
    Assertions.assertEquals(2, productItem.getProductItemImages().size());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).isMainImages());
    Assertions.assertFalse(productItem.getProductItemImages().get(0).getOriginalImage());
    Assertions.assertTrue(productItem.getProductItemImages().get(1).isMainImages());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).getOriginalImage());
  }


  @Test
  public void removeDeletedProductItemImagesTest() {
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2, productItemImage3)));
    service.removeDeletedProductItemImages(productItem);
    Assertions.assertEquals(2, productItem.getProductItemImages().size());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).isMainImages());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).getOriginalImage());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).isMainImages());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).getOriginalImage());
  }

  @Test
  public void removeDeletedProductItemImagesDuplicateMainImageTest() {
    ReflectionTestUtils.setField(service, "uniqueMainImageAtItemLevel", true);
    productItemImage1.setMainImages(true);
    productItemImage1.setActive(true);
    productItemImage2.setMainImages(true);
    productItemImage2.setActive(true);
    productItemImage3.setMainImages(true);
    productItemImage3.setActive(true);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2, productItemImage3)));
    service.removeDeletedProductItemImages(productItem);
    Assertions.assertEquals(2, productItem.getProductItemImages().size());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).isMainImages());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).getOriginalImage());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).isMainImages());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).getOriginalImage());
  }

  @Test
  public void removeDeletedProductItemImagesWithMultipleMainImagesTest() {
    productItemImage2.setMainImages(true);
    productItemImage2.setMarkForDelete(false);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2)));
    service.removeDeletedProductItemImages(productItem);
    Assertions.assertEquals(2, productItem.getProductItemImages().size());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).isMainImages());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).getOriginalImage());
    Assertions.assertTrue(productItem.getProductItemImages().get(1).isMainImages());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).getOriginalImage());
  }

  @Test
  public void removeDeletedProductItemImagesWithMultipleMainAndOriginalImagesTest() {
    productItemImage1.setOriginalImage(true);
    productItemImage2.setMainImages(true);
    productItemImage2.setOriginalImage(true);
    productItemImage2.setMarkForDelete(false);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2)));
    service.removeDeletedProductItemImages(productItem);
    Assertions.assertEquals(1, productItem.getProductItemImages().size());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).isMainImages());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).getOriginalImage());
  }

  @Test
  public void removeDeletedProductItemImagesWithMultipleMainAndNonOriginalImagesTest() {
    productItemImage1.setOriginalImage(false);
    productItemImage2.setMainImages(true);
    productItemImage2.setOriginalImage(false);
    productItemImage2.setMarkForDelete(false);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2)));
    service.removeDeletedProductItemImages(productItem);
    Assertions.assertEquals(2, productItem.getProductItemImages().size());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).isMainImages());
    Assertions.assertFalse(productItem.getProductItemImages().get(0).getOriginalImage());
    Assertions.assertTrue(productItem.getProductItemImages().get(1).isMainImages());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).getOriginalImage());
  }

  @Test
  public void removeDeletedProductItemImagesWithNoMainImageTest() {
    productItemImage1.setMainImages(false);
    productItemImage1.setSequence(0);
    productItemImage2.setMainImages(false);
    productItemImage2.setSequence(1);
    productItemImage2.setMarkForDelete(false);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2)));
    service.removeDeletedProductItemImages(productItem);
    Assertions.assertEquals(2, productItem.getProductItemImages().size());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).isMainImages());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).getOriginalImage());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).isMainImages());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).getOriginalImage());
  }

  @Test
  public void removeDeletedProductItemImagesWithNoMainImageConsiderAnyImageAsMainImageInCaseOfMainImageNotPresentTrueTest() {
    ReflectionTestUtils.setField(service, "considerAnyImageAsMainImageInCaseOfMainImageNotPresent", true);
    productItemImage1.setMainImages(false);
    productItemImage1.setSequence(1);
    productItemImage2.setMainImages(false);
    productItemImage2.setSequence(0);
    productItemImage2.setMarkForDelete(false);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1, productItemImage2)));
    service.removeDeletedProductItemImages(productItem);
    Assertions.assertEquals(2, productItem.getProductItemImages().size());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).isMainImages());
    Assertions.assertTrue(productItem.getProductItemImages().get(0).getOriginalImage());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).isMainImages());
    Assertions.assertFalse(productItem.getProductItemImages().get(1).getOriginalImage());
  }

  @Test
  public void removeDeletedProductItemImagesWithSequenceAsNullTest() {
    ReflectionTestUtils.setField(service, "considerAnyImageAsMainImageInCaseOfMainImageNotPresent", true);
    productItemImage1.setMainImages(false);
    productItemImage1.setSequence(null);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList(productItemImage1)));
    service.removeDeletedProductItemImages(productItem);
    Assertions.assertEquals(1, productItem.getProductItemImages().size());
    Assertions.assertFalse(productItem.getProductItemImages().get(0).isMainImages());
  }

  @Test
  public void removeDeletedProductItemImagesWithEmptyProductImagesTest() {
    ReflectionTestUtils.setField(service, "considerAnyImageAsMainImageInCaseOfMainImageNotPresent", true);
    productItem.setProductItemImages(
        new ArrayList<>(Arrays.asList()));
    service.removeDeletedProductItemImages(productItem);
    Assertions.assertEquals(0, productItem.getProductItemImages().size());
  }

  @Test
  public void getProductItemImagesCachedTest() {
    Product product = new Product();
    product.setProductItems(Collections.singletonList(productItem));
    ReflectionTestUtils.setField(service,"productItemImagesPartitionSize",1);
    service.getProductItemImagesCached(STORE_ID, product);
    verify(imageService).getDetachedProductItemImagesByStoreIdAndProductItemIds(
        STORE_ID, Collections.singletonList(productItem.getId()));
  }

  @Test
  public void getProductItemAttributeValuesWithAttributesCachedTest() {
    Product product = new Product();
    product.setProductItems(Collections.singletonList(productItem));
    service.getProductItemAttributeValuesWithAttributesCached(STORE_ID, product);
    verify(productItemAttributeValueService).getDetachedProductItemAttributeValuesByStoreIdAndProductItemIds(
        STORE_ID, Collections.singletonList(productItem.getId()));
  }

  @Test
  public void getProductItemsByStoreIdAndIdsTest() {
    Pageable pageable = PageRequest.of(0, 100);
    service.getProductItemsByStoreIdAndIds(STORE_ID, Arrays.asList(PRODUCT_ITEM_ID_1, PRODUCT_ITEM_ID_2), pageable);
    verify(repository).findByStoreIdAndIdIn(
        STORE_ID, Arrays.asList(PRODUCT_ITEM_ID_1, PRODUCT_ITEM_ID_2), pageable);
  }

  @Test
  public void updateProductItemUpcCodeTest() throws Exception {
    ProductItemUpcCodeUpdateRequest productItemUpcCodeUpdateRequests =
        ProductItemUpcCodeUpdateRequest.builder().upcCode("upcCode").skuCode(SKU_CODE).build();
    ProductItem savedProductItem = new ProductItem();
    BeanUtils.copyProperties(this.productItem, savedProductItem);
    Mockito.when(this.repository.findById(this.productItem.getId())).thenReturn(Optional.of(savedProductItem));
    Mockito.when(repository.findByStoreIdAndSkuCodeAndMarkForDeleteFalse(STORE_ID, SKU_CODE)).thenReturn(productItem);
    Mockito.when(productRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(product);
    service.updateProductItemUpcCode(STORE_ID, Arrays.asList(productItemUpcCodeUpdateRequests), PRODUCT_CODE);
    verify(this.repository, times(1)).findById(this.productItem.getId());
    verify(this.repository, times(1)).saveAndFlush(this.productItem);
    verify(repository).findByStoreIdAndSkuCodeAndMarkForDeleteFalse(STORE_ID, SKU_CODE);
  }

  @Test
  public void updateProductItemUpcCodeEmptyRequestTest() throws Exception {
    service.updateProductItemUpcCode(STORE_ID, new ArrayList<>(), PRODUCT_CODE);
  }

  @Test
  public void updateProductItemUpcCodeProductObjectNullTest() throws Exception {
    ProductItemUpcCodeUpdateRequest productItemUpcCodeUpdateRequests =
        ProductItemUpcCodeUpdateRequest.builder().upcCode("upcCode").skuCode(SKU_CODE).build();
    ProductItem savedProductItem = new ProductItem();
    BeanUtils.copyProperties(this.productItem, savedProductItem);
    Mockito.when(this.repository.findById(this.productItem.getId())).thenReturn(Optional.of(savedProductItem));
    Mockito.when(repository.findByStoreIdAndSkuCodeAndMarkForDeleteFalse(STORE_ID, SKU_CODE)).thenReturn(productItem);
    Mockito.when(productRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(null);
    service.updateProductItemUpcCode(STORE_ID, Arrays.asList(productItemUpcCodeUpdateRequests), PRODUCT_CODE);
    verify(this.repository, times(1)).findById(this.productItem.getId());
    verify(this.repository, times(1)).saveAndFlush(this.productItem);
    verify(repository).findByStoreIdAndSkuCodeAndMarkForDeleteFalse(STORE_ID, SKU_CODE);
  }

  @Test
  public void getItemNameByUPCCodeAndProductCodeTest() {
    Mockito.when(repository.findItemNameByWithUPCCodeAndProductCodeMarkForDeleteFalse(UPC_CODE, PRODUCT_CODE, SKU_CODE))
        .thenReturn(new ArrayList<>());
    service.getItemNameByUPCCodeAndProductCode(UPC_CODE, PRODUCT_CODE, SKU_CODE);
    Mockito.verify(repository)
        .findItemNameByWithUPCCodeAndProductCodeMarkForDeleteFalse(UPC_CODE, PRODUCT_CODE, SKU_CODE);
  }

  @Test
  public void getProductItemIdsBySkuCodeTest() {
    List<String> skuCodeList = Arrays.asList(SKU_CODE);
    Mockito.when(repository.findByStoreIdAndSkuCodeIn(STORE_ID, skuCodeList)).thenReturn(productItems);
    service.getProductItemIdsBySkuCode(STORE_ID, skuCodeList);
    Mockito.verify(repository)
        .findByStoreIdAndSkuCodeIn(STORE_ID, skuCodeList);
  }

  @Test
  public void getProductItemIdsBySkuCodeStoreIdEmptyTest() {
    List<String> skuCodeList = Arrays.asList(SKU_CODE);
    Map<String, String> skuCodesAndProductItemIdsMap = service.getProductItemIdsBySkuCode(null, skuCodeList);
    Assertions.assertEquals(skuCodesAndProductItemIdsMap.size(), 0);
  }

  @Test
  public void getProductItemIdsBySkuCodeListOfSkuEmptyTest() {
    Map<String, String> skuCodesAndProductItemIdsMap = service.getProductItemIdsBySkuCode(STORE_ID, null);
    Assertions.assertEquals(skuCodesAndProductItemIdsMap.size(), 0);
  }

  @Test
  public void getProductItemIdsBySkuCodeNullStoreIdAndSkusListTest() {
    Map<String, String> skuCodesAndProductItemIdsMap = service.getProductItemIdsBySkuCode(null, null);
    Assertions.assertEquals(skuCodesAndProductItemIdsMap.size(), 0);
  }

  @Test
  public void findItemsBySkuCodesAndMarkForDeleteFalseTest() {
    Mockito.when(this.repository.findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(STORE_ID, skuCodes))
        .thenReturn(Collections.singletonList(productItem));
    List<ProductItem> response = service.findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID, skuCodes);
    Mockito.verify(this.repository).findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(STORE_ID, skuCodes);
    Assertions.assertEquals(PRODUCT_ITEM_ID_1, response.get(0).getId());
    Assertions.assertEquals(SKU_CODE, response.get(0).getSkuCode());
  }

  @Test
  public void findItemsBySkuCodesAndMarkForDeleteFalse_emptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.findItemsBySkuCodesAndMarkForDeleteFalse(StringUtils.EMPTY, skuCodes));
  }

  @Test
  public void findItemsBySkuCodesAndMarkForDeleteFalse_emptyRequestTest() {
    List<ProductItem> response = service.findItemsBySkuCodesAndMarkForDeleteFalse(STORE_ID, Collections.emptyList());
    Assertions.assertTrue(CollectionUtils.isEmpty(response));
  }

  @Test
  public void saveProductItemListTest() {
    Mockito.when(this.repository.save(productItem)).thenReturn(productItem);
    Mockito.doNothing().when(this.applicationCacheServiceBean)
        .evictProductItemsCacheByStoreIdAndProductId(productItem.getStoreId(), productItem.getProductId());
    service.saveProductItem(productItem);
    Mockito.verify(this.repository).save(productItem);
  }

  @Test
  public void getProductItemBySkuCodeTest() {
    when(repository.findByStoreIdAndSkuCode(STORE_ID, SKU_CODE)).thenReturn(productItem);
    service.getProductItemBySkuCode(STORE_ID, SKU_CODE);
    verify(repository).findByStoreIdAndSkuCode(STORE_ID, SKU_CODE);
    Assertions.assertEquals(SKU_CODE, productItem.getSkuCode());
  }

  @Test
  public void getProductItemBySkuCodeNullTest() {
    ProductItem productItem2 = service.getProductItemBySkuCode(STORE_ID, SKU_CODE);
    verify(repository).findByStoreIdAndSkuCode(STORE_ID, SKU_CODE);
    Assertions.assertNull(productItem2);
  }

  @Test
  public void getProductItemBySkuCodeNonNullTest() {
    ReflectionTestUtils.setField(this.service, "ranchIntegrationEnabled", true);
    productItem.setProductItemAttributeValues(Arrays.asList(new ProductItemAttributeValue()));
    when(repository.findByStoreIdAndSkuCode(STORE_ID, SKU_CODE)).thenReturn(productItem);
    service.getProductItemBySkuCode(STORE_ID, SKU_CODE);
    verify(repository).findByStoreIdAndSkuCode(STORE_ID, SKU_CODE);
    Assertions.assertEquals(SKU_CODE, productItem.getSkuCode());
  }

  @Test
  public void getProductItemBySkuCodeRanchTest() {
    ReflectionTestUtils.setField(this.service, "ranchIntegrationEnabled", true);
    Set<String> distributionSellerList = new HashSet<>();
    distributionSellerList.add(SKU_CODE_1);
    ReflectionTestUtils.setField(this.service, "distributionSellerList", distributionSellerList);
    productItem.setCreatedMerchant(SKU_CODE_1);
    productItem.setProductItemAttributeValues(Arrays.asList(new ProductItemAttributeValue()));
    when(repository.findByStoreIdAndSkuCode(STORE_ID, SKU_CODE)).thenReturn(productItem);
    service.getProductItemBySkuCode(STORE_ID, SKU_CODE);
    verify(repository).findByStoreIdAndSkuCode(STORE_ID, SKU_CODE);
    Assertions.assertEquals(SKU_CODE, productItem.getSkuCode());
  }

  @Test
  public void getItemCodeByUPCCodeAndProductCodeEmptySkuCodesTest() {
    when(repository.findItemCodeByUPCCodeAndProductCodeAndMarkForDeleteFalse(Arrays.asList(UPC_CODE),
        PRODUCT_CODE)).thenReturn(Arrays.asList(SKU_CODE));
    List<String> response =
        service.getItemCodeByUPCCodeAndProductCode(Arrays.asList(UPC_CODE), PRODUCT_CODE, new ArrayList<>());
    verify(repository).findItemCodeByUPCCodeAndProductCodeAndMarkForDeleteFalse(Arrays.asList(UPC_CODE), PRODUCT_CODE);
    Assertions.assertEquals(SKU_CODE, response.get(0));
  }

  @Test
  public void getItemCodeByUPCCodeAndProductCodeTest() {
    when(repository.findItemCodeByUPCCodeAndProductCodeAndSkuCodesInMarkForDeleteFalse(Arrays.asList(UPC_CODE),
        PRODUCT_CODE, Arrays.asList(SKU_CODE))).thenReturn(Arrays.asList(SKU_CODE));
    List<String> response =
        service.getItemCodeByUPCCodeAndProductCode(Arrays.asList(UPC_CODE), PRODUCT_CODE, Arrays.asList(SKU_CODE));
    verify(repository).findItemCodeByUPCCodeAndProductCodeAndSkuCodesInMarkForDeleteFalse(Arrays.asList(UPC_CODE),
        PRODUCT_CODE, Arrays.asList(SKU_CODE));
    Assertions.assertEquals(SKU_CODE, response.get(0));
  }

  @Test
  public void getBySkuCodeByProductItemIdsEmptyTest() {
    service.getBySkuCodeByProductItemIds(STORE_ID, new ArrayList<>());
    service.getBySkuCodeByProductItemIds(StringUtils.EMPTY, Collections.singletonList(PRODUCT_ITEM_ID_1));
  }

  @Test
  public void getBySkuCodeByProductItemIdsTest() {
    Mockito.when(
            repository.findByStoreIdAndIdIn(STORE_ID, Collections.singletonList(PRODUCT_ITEM_ID_1), PageRequest.of(0, 1)))
        .thenReturn(new PageImpl<>(productItems));
    service.getBySkuCodeByProductItemIds(STORE_ID, Collections.singletonList(PRODUCT_ITEM_ID_1));
    Mockito.verify(repository)
        .findByStoreIdAndIdIn(STORE_ID, Collections.singletonList(PRODUCT_ITEM_ID_1), PageRequest.of(0, 1));
  }
}
