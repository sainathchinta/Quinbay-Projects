package com.gdn.x.product.dao.api;

import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;

import com.gdn.x.product.model.entity.Product;

public interface ProductRepository
    extends MongoRepository<Product, String>, ProductRepositoryCustom {

  @Query(fields = "{'productSku' : 1, 'productCode' : 1}")
  List<Product> findBy();

  @Query(fields = "{'productSku' : 1, 'productCode' : 1}")
  Page<Product> findBy(Pageable pageable);

  @Query(fields = "{'productSku' : 1, 'productCode' : 1}")
  Page<Product> findByUpdatedDateGreaterThan(Date updatedDate, Pageable pageable);

  @Query(fields = "{'productSku' : 1, 'productCode' : 1}")
  Page<Product> findByUpdatedDateLessThan(Date updatedDate, Pageable pageable);

  /**
   * method for get all products between date interval
   * @param storeId must not be blank
   * @param fromDate must not be blank
   * @param tillDate must not be blank
   * @return Stream of Product
   */
  Stream<Product> streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(String storeId, Date fromDate, Date tillDate);

  @Query(fields = "{'productSku' : 1, 'productCode' : 1,'isSynchronized' : 1}")
  List<Product> findByMarkForDeleteFalse();

  @Query(fields = "{'productSku' : 1, 'productCode' : 1,'isSynchronized' : 1}")
  List<Product> findByMarkForDeleteFalseAndProductCodeNotNull();

  @Query(fields = "{'productSku' : 1, 'productCode' : 1,'isSynchronized' : 1}")
  List<Product> findByMarkForDeleteFalseAndProductCodeNull();

  /**
   * method for get all products
   * @param storeId must not be blank
   * @return Stream of Product
   */
  Stream<Product> streamAllByStoreId(String storeId);

  /**
   * method for get products based on given productSkus
   * @param storeId must not be blank
   * @param productSkus must not be empty
   * @return Stream of Product
   */
  Stream<Product> streamAllByStoreIdAndProductSkuIn(String storeId, List<String> productSkus);

  /**
   * @param storeId
   * @param productCatentryIds
   * @return
   */
  List<Product> findByStoreIdAndProductCatentryIdInAndMarkForDeleteFalse(String storeId,
      Set<String> productCatentryIds);


  /**
   * @param storeId
   * @param productCode
   * @return
   */
  List<Product> findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
      String storeId, String productCode);

  /**
   * @param storeId
   * @param productCodes
   * @return
   */
  List<Product> findByStoreIdAndProductCodeInAndMarkForDeleteFalse(String storeId,
      Set<String> productCodes);

  List<Product> findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId,
      String productCode);

  /**
   *
   * @param storeId
   * @param productCode
   * @return
   */
  List<Product> findByStoreIdAndProductCode(String storeId, String productCode);

  /**
   * @param storeId
   * @param productSku
   * @return
   */
  List<Product> findByStoreIdAndProductSkuIn(String storeId, List<String> productSku);

  /**
   * @param storeId
   * @param productSku
   * @return
   */
  Product findByStoreIdAndProductSku(String storeId, String productSku);

  /**
   * @param storeId
   * @param productCode
   * @param merchantCode
   * @return
   */
  List<Product> findByStoreIdAndProductCodeAndMerchantCode(String storeId, String productCode, String merchantCode);

  /**
   * @param storeId
   * @param productSku
   * @return
   */
  List<Product> findProductByStoreIdAndProductSkuInAndMarkForDeleteFalse(String storeId,
      Set<String> productSku);

  @Query(fields = "{'productSku' : 1, 'productCode' : 1}")
  List<Product> findProductCodeByStoreIdAndProductSkuIn(String storeId, List<String> productSku);

  /**
   * @param storeId must not be blank
   * @param merchantCode must not be blank
   * @return set of unique products
   */
  Set<Product> findProductsByStoreIdAndMerchantCodeAndMarkForDeleteFalse(String storeId,
      String merchantCode);

  Long countByStoreIdAndMasterDataProductBrandIgnoreCase(String storeId, String brand);

  Long countByStoreIdAndMasterDataProductBrandIgnoreCaseAndIsSynchronizedFalse(String storeId, String brand);

  /**
   * Get Products by productSkus, storeId, mark for delete false and synchronized true
   * @param storeId
   * @param productSkus
   * @return
   */
  List<Product> findProductByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsSynchronizedTrue(String storeId,
      Set<String> productSkus);

  /**
   * Delete By StoreId And ProductSkus
   *
   * @param storeId
   * @param productSku
   * @return
   */
  List<Product> deleteByStoreIdAndProductSkuIn(String storeId, Set<String> productSku);

  /**
   *
   * Fetch all the suspended products
   *
   * @param storeId
   * @param merchantCode
   * @param suspended
   * @return
   */
  List<Product> findProductsByStoreIdAndMerchantCodeAndIsSuspended(String storeId,
      String merchantCode, boolean suspended);

  Page<Product> findByUpdatedDateBetween(Date fromDate, Date tillDate, Pageable pageable);

  Product findByProductCodeAndMerchantCode(String productCode, String sellerCode);

  List<Product> findFirst2ByStoreIdAndProductCode(String storeId, String productCode);

  List<Product> findByStoreIdAndProductCodeIn(String storeId, Set<String> productCodes);

  List<Product> findFirst2ByStoreIdAndProductCodeAndMarkForDelete(String storeId, String productCode,
      boolean markForDelete);
}
