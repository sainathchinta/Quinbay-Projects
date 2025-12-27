package com.gdn.mta.product.repository;

import java.util.List;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import org.springframework.stereotype.Repository;

import com.gda.mta.product.dto.ItemPickupPointDto;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;

import lombok.extern.slf4j.Slf4j;

@Repository
@Slf4j
public class ProductItemWholesalePriceCustomRepositoryImpl implements ProductItemWholesalePriceCustomRepository {
  private static final String STORE_ID = "storeId";
  private static final String ITEMS_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public List<ProductItemWholesalePrice> findProductItemWholesalePriceByItemSkuAndPickupPointCode(String storeId,
      List<ItemPickupPointDto> itemPickupPointDtoList) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<ProductItemWholesalePrice> criteriaQuery =
        criteriaBuilder.createQuery(ProductItemWholesalePrice.class);
    Root<ProductItemWholesalePrice> productItemWholesalePriceRoot = criteriaQuery.from(ProductItemWholesalePrice.class);
    Predicate storeIdPredicate = criteriaBuilder.equal(productItemWholesalePriceRoot.get(STORE_ID), storeId);
    Predicate[] itemSkuAndPickupPointPredicates = new Predicate[itemPickupPointDtoList.size()];
    int index = 0;
    for (ItemPickupPointDto itemPickupPointDto : itemPickupPointDtoList) {
      Predicate predicate =
          criteriaBuilder.equal(productItemWholesalePriceRoot.get(ITEMS_SKU), itemPickupPointDto.getItemSku());
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.equal(productItemWholesalePriceRoot.get(PICKUP_POINT_CODE),
              itemPickupPointDto.getPickupPointCode()));
      itemSkuAndPickupPointPredicates[index++] = predicate;
    }
    Predicate finalPredicate =
        criteriaBuilder.and(storeIdPredicate, criteriaBuilder.or(itemSkuAndPickupPointPredicates));
    CriteriaQuery<ProductItemWholesalePrice> resultCriteriaQuery =
        criteriaQuery.select(productItemWholesalePriceRoot).where(finalPredicate);
    List<ProductItemWholesalePrice> productItemWholesalePriceList =
        entityManager.createQuery(resultCriteriaQuery).getResultList();
    return productItemWholesalePriceList;
  }
}
