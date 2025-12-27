package com.gdn.x.mta.distributiontask.dao.api;

import java.util.Date;
import java.util.List;


import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;

/**
 * Created by virajjasani on 15/09/16.
 */
public interface ProductDistributionTaskRepository
    extends JpaRepository<ProductDistributionTask, String> {

  @Query(value = "select p "
      + "from ProductDistributionTask p where p.state = com.gdn.x.mta.distributiontask.model.type.WorkflowState.IN_REVIEW "
      + "and p.slaDateExceed = :slaDateExceed " + "and p.markForDelete = false "
      + "and :currentDate > p.slaDate")
  List<ProductDistributionTask> getListOfProductDistributionTaskSLA(
      @Param("currentDate") Date currentDate, @Param("slaDateExceed") boolean slaDateExceed);

  @Query(value = "SELECT * FROM PDT_PRODUCT_DISTRIBUTION_TASK WHERE ID = ?1 AND "
      + "MARK_FOR_DELETE IS FALSE AND STATE='IN_REVIEW'", nativeQuery = true)
  ProductDistributionTask getProductDistributionTaskByIdAndStillInReview(String id);
  
  ProductDistributionTask findTopByProductProductCodeOrderByUpdatedDateDesc(String productCode);
  
  ProductDistributionTask findProductDistributionTaskByProductProductCodeAndMarkForDeleteFalse(String productCode);

  @Query(value = "SELECT * FROM PDT_PRODUCT_DISTRIBUTION_TASK WHERE PRODUCT = ?1 AND "
      + "MARK_FOR_DELETE IS FALSE", nativeQuery = true)
  ProductDistributionTask getProductDistributionTaskByProduct(String productId);

  @Query(value = "SELECT * FROM PDT_PRODUCT_DISTRIBUTION_TASK WHERE PRODUCT IN "
      + ":IDS AND MARK_FOR_DELETE IS FALSE", nativeQuery = true)
  List<ProductDistributionTask> getStatusForProducts(@Param("IDS") List<String> productIdList);

  @Query(
      value = "SELECT * FROM PDT_PRODUCT_DISTRIBUTION_TASK WHERE VENDOR = :VENDOR_ID AND PRODUCT IN "
          + ":IDS AND MARK_FOR_DELETE IS FALSE",
      nativeQuery = true)
  List<ProductDistributionTask> getStatusForProducts(@Param("VENDOR_ID") String vendorId,
      @Param("IDS") List<String> productIdList);

  @Query(value= "SELECT TASK_CODE FROM PDT_PRODUCT_DISTRIBUTION_TASK WHERE PRODUCT = "
      + ":ID AND MARK_FOR_DELETE IS FALSE", nativeQuery = true)
  String getTaskCodeForProduct(@Param("ID") String productId);

  @Modifying
  @Query(
      value = "update ProductDistributionTask pdt set markForDelete = true where pdt.product in :productList and pdt.markForDelete = false ")
  void updateProductDistributionTask(@Param("productList") List<Product> productList);


  @Modifying
  @Query(value = "update ProductDistributionTask pdt set markForDelete = true where pdt.vendor.id = :vendorId ")
  void clearProductsInProductDistributionTask(@Param("vendorId") String vendorId);

  @Modifying
  @Query(value = "delete from pdt_product_distribution_task pdt where pdt.product IN (:productIdList)", nativeQuery = true)
  void deleteByProductIds(@Param("productIdList") List<String> productIdList);

  List<ProductDistributionTask> findByStoreIdAndProductIdAndMarkForDeleteFalse(String storeId, String productId);
}
