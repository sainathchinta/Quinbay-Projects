package com.gdn.mta.product.repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import com.gdn.partners.pbp.commons.constants.Constants;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.gdn.mta.product.entity.ProductItemBusinessPartner;

import lombok.extern.slf4j.Slf4j;

@Repository
@Slf4j
public class ProductItemBusinessPartnerCustomRepositoryImpl implements ProductItemBusinessPartnerCustomRepository {

  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_BUSINESS_PARTNER_ID = "productBusinessPartnerId";
  private static final String ITEMS_SKU = "gdnProductItemSku";
  private static final String PICKUP_POINT_CODE = "pickupPointId";
  private static final String MARK_FOR_DELETE = "markForDelete";

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public Page<ProductItemBusinessPartner> findByStoreIdAndProductBusinessPartnerIdAndGdnProductItemSkuAndPickupPointIdIn(
    String storeId, String productBusinessPartnerId, String itemSku, Set<String> pickupPointCodes,
    Pageable pageable, boolean isFbbSortRequired) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<ProductItemBusinessPartner> criteriaQuery =
        criteriaBuilder.createQuery(ProductItemBusinessPartner.class);
    CriteriaQuery<Long> countCriteriaQuery = criteriaBuilder.createQuery(Long.class);
    Root<ProductItemBusinessPartner> productItemBusinessPartnerRoot =
        criteriaQuery.from(ProductItemBusinessPartner.class);
    Root<ProductItemBusinessPartner> countRoot =
        countCriteriaQuery.from(ProductItemBusinessPartner.class);
    Predicate predicate =
        getL5ListingPredicate(storeId, productBusinessPartnerId, criteriaBuilder, productItemBusinessPartnerRoot,
            itemSku, pickupPointCodes);
    Predicate countPredicate =
        getL5ListingPredicate(storeId, productBusinessPartnerId, criteriaBuilder, countRoot,
            itemSku, pickupPointCodes);
    CriteriaQuery<ProductItemBusinessPartner> resultCriteriaQuery =
        criteriaQuery.select(productItemBusinessPartnerRoot).where(predicate);
    if (isFbbSortRequired) {
      //Sorting fbb L5 and oldest created L5 on top
      List<Order> sortingOrderList = new ArrayList<>();
      sortingOrderList.add(
        criteriaBuilder.desc(productItemBusinessPartnerRoot.get(Constants.GDN_PRODUCT_ITEM_SKU)));
      sortingOrderList.add(criteriaBuilder.desc(productItemBusinessPartnerRoot.get(Constants.FBB_ACTIVE)));
      sortingOrderList.add(
        criteriaBuilder.asc(productItemBusinessPartnerRoot.get(Constants.CREATED_DATE)));
      resultCriteriaQuery.orderBy(sortingOrderList);
    }
    countCriteriaQuery.select(criteriaBuilder.count(countRoot))
        .where(countPredicate);
    List<ProductItemBusinessPartner> productItemBusinessPartnerList =
        entityManager.createQuery(resultCriteriaQuery).setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
            .setMaxResults(pageable.getPageSize()).getResultList();
    Long totalRecords = (Long) entityManager.createQuery(countCriteriaQuery).getSingleResult();
    return new PageImpl<>(productItemBusinessPartnerList, pageable, totalRecords);
  }

  private Predicate getL5ListingPredicate(String storeId, String productBusinessPartnerId,
      CriteriaBuilder criteriaBuilder, Root<ProductItemBusinessPartner> productItemBusinessPartnerRoot,
      String itemSku, Set<String> pickupPointCodes) {
    Predicate predicate = criteriaBuilder.equal(productItemBusinessPartnerRoot.get(STORE_ID), storeId);
    predicate = criteriaBuilder.and(predicate,
        criteriaBuilder.equal(productItemBusinessPartnerRoot.get(PRODUCT_BUSINESS_PARTNER_ID),
            productBusinessPartnerId));
    predicate = criteriaBuilder.and(predicate,
        criteriaBuilder.equal(productItemBusinessPartnerRoot.get(MARK_FOR_DELETE), false));

    if (StringUtils.isNotBlank(itemSku)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(productItemBusinessPartnerRoot.get(ITEMS_SKU), itemSku));
    }

    if (CollectionUtils.isNotEmpty(pickupPointCodes)) {
      CriteriaBuilder.In<Object> criteriaIn = criteriaBuilder.in(productItemBusinessPartnerRoot.get(PICKUP_POINT_CODE));
      criteriaIn = criteriaIn.value(pickupPointCodes);
      predicate = criteriaBuilder.and(predicate, criteriaIn);
    }
    return predicate;
  }
}
