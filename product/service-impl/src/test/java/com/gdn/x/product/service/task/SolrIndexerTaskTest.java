package com.gdn.x.product.service.task;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;


public class SolrIndexerTaskTest {

  private static final String ITEM_SKU3 = "item-sku-3";

  private static final String STORE_ID = "store_id";

  private static final String PRODUCT_SKU1 = "productSku1";

  private static final String ITEM_SKU2 = "itemsku2";

  private static final String ITEM_SKU1 = "itemsku1";

  private static final String PRODUCT_SKU2 = "product-sku-2";

  @InjectMocks
  private SolrIndexerTask task;

  @Mock
  private ProductAndItemSolrIndexerService indexerService;

  @Mock
  private ProductService productService;

  private Product product1;

  @Test
  public void constructorTest() {
    new SolrIndexerTask();
  }

  @Test
  public void hashCodeTest() {
    this.task.hashCode();
  }

  @BeforeEach
  public void init() {
    openMocks(this);

    product1 = new Product(PRODUCT_SKU1);
    product1.setStoreId(STORE_ID);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU1)).thenReturn(product1);

  }

  @Test
  public void runWithException() throws Exception {
    Item item = new Item(ITEM_SKU1, PRODUCT_SKU1);
    item.setStoreId(STORE_ID);
    this.task.setDataTobeIndexed(item);
    doThrow(Exception.class).when(this.indexerService)
        .applyProductAndItems(new ProductAndItemsVO(product1, Arrays.asList(item)), false);
    this.task.run();
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU1);
    verify(this.indexerService)
        .applyProductAndItems(new ProductAndItemsVO(product1, Arrays.asList(item)), false);
  }

  @Test
  public void runWithItemObject() throws Exception {

    Item item = new Item(ITEM_SKU1, PRODUCT_SKU1);
    item.setStoreId(STORE_ID);

    this.task.setDataTobeIndexed(item);
    this.task.run();

    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU1);
    verify(this.indexerService)
        .applyProductAndItems(new ProductAndItemsVO(product1, Arrays.asList(item)), false);
  }

  @Test
  public void runWithListOfItem() throws Exception {
    Product product1 = new Product(PRODUCT_SKU1);
    Product product2 = new Product(PRODUCT_SKU2);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU1)).thenReturn(product1);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU2)).thenReturn(product2);

    Item item1 = new Item(ITEM_SKU1, PRODUCT_SKU1);
    item1.setStoreId(STORE_ID);
    Item item2 = new Item(ITEM_SKU2, PRODUCT_SKU2);
    item2.setStoreId(STORE_ID);
    Item item3 = new Item(ITEM_SKU3, PRODUCT_SKU2);
    item3.setStoreId(STORE_ID);
    List<Item> items = Arrays.asList(item1, item2, item3);

    this.task.setDataTobeIndexed(items);

    this.task.run();
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU1);
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU2);
    verify(this.indexerService)
        .applyProductAndItems(new ProductAndItemsVO(product1, Arrays.asList(item1)), false);
    verify(this.indexerService)
        .applyProductAndItems(new ProductAndItemsVO(product2, Arrays.asList(item2, item3)), false);
  }

  @Test
  public void runWithListOfProduct() throws Exception {
    Product product1 = new Product();
    product1.setProductSku("productsku1");
    Product product2 = new Product();
    product2.setProductSku("productsku2");
    List<Product> products = new ArrayList<>();
    products.add(product1);
    products.add(product2);
    this.task.setDataTobeIndexed(products);
    this.task.run();
    verify(this.indexerService).applyProduct(product1, false);
    verify(this.indexerService).applyProduct(product2, false);
  }

  @Test
  public void runWithListOfProductAndItemsObject() throws Exception {
    List<ProductAndItemsVO> list = new ArrayList<>();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    list.add(productAndItemsVO);
    list.add(productAndItemsVO);
    this.task.setDataTobeIndexed(list);
    this.task.run();
    verify(this.indexerService, times(2)).applyProductAndItems(productAndItemsVO, false);
  }

  @Test
  public void runWithListOfProductObject() throws Exception {
    List<Product> list = new ArrayList<>();
    Product product = new Product();
    list.add(product);
    list.add(product);
    this.task.setDataTobeIndexed(list);
    this.task.run();
    verify(this.indexerService, times(2)).applyProduct(product, false);
  }

  @Test
  public void runWithListOfRandomObject() {
    ProductAndItemSolr random = new ProductAndItemSolr();
    List<ProductAndItemSolr> products = new ArrayList<>();
    products.add(random);
    this.task.setDataTobeIndexed(products);
    this.task.run();
  }

  @Test
  public void runWithMasterDataDetailWithProductAndItemsResponseVo() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo objectToIndex =
        new MasterDataDetailWithProductAndItemsResponseVo();
    this.task.setDataTobeIndexed(objectToIndex);
    this.task.run();
    verify(this.indexerService).applyMasterDataDetailWithProductAndItems(objectToIndex, false);
  }

  @Test
  public void runWithNullObject() {
    this.task.setDataTobeIndexed(null);
    this.task.run();
  }

  @Test
  public void runWithProductAndItemsObject() throws Exception {
    ProductAndItemsVO productAndItems = new ProductAndItemsVO();
    this.task.setDataTobeIndexed(productAndItems);
    this.task.run();
    verify(this.indexerService).applyProductAndItems(productAndItems, false);
  }


  @Test
  public void runWithProductObject() throws Exception {
    Product product = new Product();
    this.task.setDataTobeIndexed(product);
    this.task.run();
    verify(this.indexerService).applyProduct(product, false);
  }

  @Test
  public void runWithRandomObject() {
    ProductAndItemSolr random = new ProductAndItemSolr();
    this.task.setDataTobeIndexed(random);
    this.task.run();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.indexerService);
    verifyNoMoreInteractions(this.productService);
  }

}
