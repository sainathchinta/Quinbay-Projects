package com.gdn.x.productcategorybase.repository.impl.brand;

import com.gdn.x.productcategorybase.BrandAuthorizationWipStatus;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationWip;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthorisationWipRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Objects;

@Repository
public class BrandAuthorisationWipRepositoryImpl implements BrandAuthorisationWipRepositoryCustom {

  private static final String STORE_ID = "storeId";
  private static final String AUTH_STATUS = "authorisationStatus";
  private static final String SELLER_CODE = "sellerCode";
  private static final String MARK_FOR_DELETE = "markForDelete";
  private static final String BRAND_NAME = "brandName";
  private static final String ANY_STRING = "%";
  private static final String UPDATED_DATE = "updatedDate";
  private static final String AUTH_START_DATE = "authStartDate";
  private static final String AUTH_EXPIRE_DATE = "authExpireDate";

  @PersistenceContext
  private EntityManager entityManager;

  @Value("${brand.auth.wip.near.expiry.days.threshold}")
  private Integer brandAuthWipNearExpiryDaysThreshold;

  @Override
  public Page<BrandAuthorisationWip> findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(
      String storeId, String sellerCode, String brandName, String status, Pageable pageable) {
    //Predicate for the listing result
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<BrandAuthorisationWip> criteriaQuery = criteriaBuilder.createQuery(BrandAuthorisationWip.class);
    Root<BrandAuthorisationWip> brandAuthorisationWipRoot = criteriaQuery.from(BrandAuthorisationWip.class);
    Predicate contentPredicate =
        getPredicateForBrandAuthorisationWipListing(storeId, sellerCode, brandName, status,
            criteriaBuilder, brandAuthorisationWipRoot);

    //Predicate for total count of the result
    CriteriaQuery<Long> countCriteriaQuery = criteriaBuilder.createQuery(Long.class);
    Root<BrandAuthorisationWip> countRoot = countCriteriaQuery.from(BrandAuthorisationWip.class);

    criteriaQuery.select(brandAuthorisationWipRoot).where(contentPredicate);
    criteriaQuery.orderBy(criteriaBuilder.desc(brandAuthorisationWipRoot.get(UPDATED_DATE)));

    if (Objects.isNull(contentPredicate)) {
      return new PageImpl<>(new ArrayList<>(), pageable, 0);
    }

    // Execute the result query with pagination
    List<BrandAuthorisationWip> pagingContent = entityManager.createQuery(criteriaQuery)
        .setFirstResult((int) pageable.getOffset())
        .setMaxResults(pageable.getPageSize())
        .getResultList();

    Predicate countPredicate =
        getPredicateForBrandAuthorisationWipListing(storeId, sellerCode, brandName, status,
            criteriaBuilder, countRoot);
    // Apply the predicate to the count query and select the count
    countCriteriaQuery.select(criteriaBuilder.count(countRoot)).where(countPredicate);
    Long totalRecords = entityManager.createQuery(countCriteriaQuery).getSingleResult();

    // Return the paginated result
    return new PageImpl<>(pagingContent, pageable, totalRecords);
  }

  @Override
  public Page<BrandAuthorisationWip> findBrandAuthorisationWipForExternalListing(String storeId,
      String sellerCode, String brandName, String tabName, List<BrandAuthorizationWipStatus> status,
      Pageable pageable) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<BrandAuthorisationWip> criteriaQuery =
        criteriaBuilder.createQuery(BrandAuthorisationWip.class);
    Root<BrandAuthorisationWip> brandAuthorisationWipRoot =
        criteriaQuery.from(BrandAuthorisationWip.class);
    Predicate predicate =
        getPredicateForBrandAuthorisationWipListingPage(storeId, sellerCode, brandName, status,
            criteriaBuilder, brandAuthorisationWipRoot);

    //Predicate for total count of the result
    CriteriaQuery<Long> countCriteriaQuery = criteriaBuilder.createQuery(Long.class);
    Root<BrandAuthorisationWip> countRoot = countCriteriaQuery.from(BrandAuthorisationWip.class);

    criteriaQuery.select(brandAuthorisationWipRoot).where(predicate);
    if (Constants.UNDER_REVIEW.equals(tabName)) {
      criteriaQuery.orderBy(criteriaBuilder.desc(brandAuthorisationWipRoot.get(UPDATED_DATE)));
    } else if (Constants.AUTHORIZED_BRAND.equals(tabName)) {
      criteriaQuery.orderBy(criteriaBuilder.asc(brandAuthorisationWipRoot.get(AUTH_STATUS)));
    }

    if (Objects.isNull(predicate)) {
      return new PageImpl<>(new ArrayList<>(), pageable, 0);
    }

    // Execute the result query with pagination
    List<BrandAuthorisationWip> pagingContent =
        entityManager.createQuery(criteriaQuery).setFirstResult((int) pageable.getOffset())
            .setMaxResults(pageable.getPageSize()).getResultList();

    // Apply the predicate to the count query and select the count
    Predicate countPredicate =
        getPredicateForBrandAuthorisationWipListingPage(storeId, sellerCode, brandName, status,
            criteriaBuilder, countRoot);
    countCriteriaQuery.select(criteriaBuilder.count(countRoot)).where(countPredicate);
    Long totalRecords = entityManager.createQuery(countCriteriaQuery).getSingleResult();

    // Return the paginated result
    return new PageImpl<>(pagingContent, pageable, totalRecords);
  }

  private Predicate getPredicateForBrandAuthorisationWipListing(String storeId, String sellerCode,
      String brandName, String status, CriteriaBuilder criteriaBuilder,
      Root<BrandAuthorisationWip> brandAuthorisationWipRoot) {
    Predicate predicate = criteriaBuilder.equal(brandAuthorisationWipRoot.get(STORE_ID), storeId);
    predicate = criteriaBuilder.and(predicate,
        criteriaBuilder.equal(brandAuthorisationWipRoot.get(MARK_FOR_DELETE), false));
    if (StringUtils.isNotBlank(sellerCode)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.equal(brandAuthorisationWipRoot.get(SELLER_CODE), sellerCode));
    }
    if (StringUtils.isNotEmpty(brandName)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.like(criteriaBuilder.lower(brandAuthorisationWipRoot.get(BRAND_NAME)),
              ANY_STRING + brandName.toLowerCase() + ANY_STRING));
    }
    if (StringUtils.isNotBlank(status)) {
      if (BrandAuthorizationWipStatus.NEAR_EXPIRY.name().equals(status)) {
        predicate = criteriaBuilder.and(predicate,
            criteriaBuilder.equal(brandAuthorisationWipRoot.get(AUTH_STATUS),
                BrandAuthorizationWipStatus.IN_REVIEW));
        Calendar calendar = Calendar.getInstance();
        calendar.add(Calendar.DATE, brandAuthWipNearExpiryDaysThreshold);
        Date currDatePlusThreshold = calendar.getTime();
        predicate = criteriaBuilder.and(predicate,
            criteriaBuilder.greaterThan(criteriaBuilder.literal(currDatePlusThreshold),
                brandAuthorisationWipRoot.get(AUTH_START_DATE).as(Date.class)));
      }
      else {
        predicate = criteriaBuilder.and(predicate,
            criteriaBuilder.equal(brandAuthorisationWipRoot.get(AUTH_STATUS),
                BrandAuthorizationWipStatus.valueOf(status)));
      }
    }
    else {
      List<BrandAuthorizationWipStatus> validStatuses =
          List.of(BrandAuthorizationWipStatus.IN_REVIEW, BrandAuthorizationWipStatus.NEED_REVISION,
              BrandAuthorizationWipStatus.REJECTED);
      CriteriaBuilder.In<BrandAuthorizationWipStatus> inClause =
          criteriaBuilder.in(brandAuthorisationWipRoot.get(AUTH_STATUS));
      validStatuses.forEach(inClause::value);
      predicate = criteriaBuilder.and(predicate, inClause);
    }
    return predicate;
  }

  private Predicate getPredicateForBrandAuthorisationWipListingPage(String storeId,
      String sellerCode, String brandName, List<BrandAuthorizationWipStatus> status,
      CriteriaBuilder criteriaBuilder, Root<BrandAuthorisationWip> brandAuthorisationWipRoot) {
    Predicate predicate = criteriaBuilder.equal(brandAuthorisationWipRoot.get(STORE_ID), storeId);
    predicate = criteriaBuilder.and(predicate,
        criteriaBuilder.equal(brandAuthorisationWipRoot.get(MARK_FOR_DELETE), false));
    if (StringUtils.isNotBlank(sellerCode)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.equal(brandAuthorisationWipRoot.get(SELLER_CODE), sellerCode));
    }
    if (StringUtils.isNotEmpty(brandName)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.like(criteriaBuilder.lower(brandAuthorisationWipRoot.get(BRAND_NAME)),
              ANY_STRING + brandName.toLowerCase() + ANY_STRING));
    }
    /* Status size check because if status filter applied it will contain only one value, else it
     should hold other statuses as well */

    CriteriaBuilder.In<BrandAuthorizationWipStatus> notInClause =
        criteriaBuilder.in(brandAuthorisationWipRoot.get(AUTH_STATUS));
    notInClause.value(BrandAuthorizationWipStatus.NEED_REVISION);
    notInClause.value(BrandAuthorizationWipStatus.REJECTED);
    notInClause.value(BrandAuthorizationWipStatus.IN_REVIEW);

    // Build the predicate based on the status size and its contents
    if (status.size() == Constants.ONE) {
      // Handle the specific cases for INACTIVE, EXPIRED, ACTIVE, and others
      if (status.contains(BrandAuthorizationWipStatus.INACTIVE)) {
        predicate = criteriaBuilder.and(predicate,
            criteriaBuilder.lessThan(criteriaBuilder.literal(new Date()),
                brandAuthorisationWipRoot.get(AUTH_START_DATE).as(Date.class)),
            criteriaBuilder.not(notInClause));
      } else if (status.contains(BrandAuthorizationWipStatus.EXPIRED)) {
        predicate = criteriaBuilder.and(predicate,
            criteriaBuilder.greaterThan(criteriaBuilder.literal(new Date()),
                brandAuthorisationWipRoot.get(AUTH_EXPIRE_DATE).as(Date.class)),
            criteriaBuilder.not(notInClause));
      } else if (status.contains(BrandAuthorizationWipStatus.ACTIVE)) {
        predicate = criteriaBuilder.and(predicate,
            criteriaBuilder.equal(brandAuthorisationWipRoot.get(AUTH_STATUS),
                BrandAuthorizationWipStatus.valueOf(status.getFirst().name())),
            criteriaBuilder.between(criteriaBuilder.literal(new Date()),
                brandAuthorisationWipRoot.get(AUTH_START_DATE).as(Date.class),
                brandAuthorisationWipRoot.get(AUTH_EXPIRE_DATE).as(Date.class)),
            criteriaBuilder.not(notInClause));
      } else if (status.contains(BrandAuthorizationWipStatus.NEED_REVISION) || status.contains(
          BrandAuthorizationWipStatus.REJECTED) || status.contains(
          BrandAuthorizationWipStatus.IN_REVIEW)) {
        predicate = criteriaBuilder.and(predicate,
            criteriaBuilder.equal(brandAuthorisationWipRoot.get(AUTH_STATUS),
                BrandAuthorizationWipStatus.valueOf(status.getFirst().name())));
      }
    } else {
      // Handle the case for multiple statuses where status.size() > 1
      CriteriaBuilder.In<BrandAuthorizationWipStatus> inClause =
          criteriaBuilder.in(brandAuthorisationWipRoot.get(AUTH_STATUS));
      for (BrandAuthorizationWipStatus wipStatus : status) {
        inClause.value(BrandAuthorizationWipStatus.valueOf(wipStatus.name()));
      }
      predicate = criteriaBuilder.and(predicate, inClause);
    }
    return predicate;
  }
}