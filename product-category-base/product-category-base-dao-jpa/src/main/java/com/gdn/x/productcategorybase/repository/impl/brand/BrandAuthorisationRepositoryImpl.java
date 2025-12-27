package com.gdn.x.productcategorybase.repository.impl.brand;

import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthorisationRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import org.apache.commons.lang.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

@Repository
public class BrandAuthorisationRepositoryImpl implements BrandAuthorisationRepositoryCustom {

  private static final String STORE_ID = "storeId";
  private static final String AUTH_STATUS = "authorisationStatus";
  private static final String SELLER_CODE = "sellerCode";
  private static final String MARK_FOR_DELETE = "markForDelete";
  private static final String BRAND_NAME = "brandName";
  private static final String ANY_STRING = "%";
  private static final String CREATED_DATE = "createdDate";
  private static final String AUTH_START_DATE = "authStartDate";
  private static final String AUTH_EXPIRE_DATE = "authExpireDate";
  private static final String AUTHORISATION_STATUS = "authorisationStatus";

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public Page<BrandAuthorisation> findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(
      String storeId, String sellerCode, String brandName, String status, Pageable pageable,
      int brandAuthNearExpiryDaysThreshold) {

    // Handle the status parameter
    status = Optional.ofNullable(status).orElse(StringUtils.EMPTY).toUpperCase();

    // Obtain CriteriaBuilder from EntityManager
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();

    // Create the result query for fetching entities
    CriteriaQuery<BrandAuthorisation> criteriaQuery = criteriaBuilder.createQuery(BrandAuthorisation.class);
    Root<BrandAuthorisation> brandAuthorisationRoot = criteriaQuery.from(BrandAuthorisation.class);

    // Create a Predicate using a helper method
    Predicate predicate =
        getPredicateForBrandAuthorisationListing(storeId, sellerCode, brandName, status,
            criteriaBuilder, brandAuthorisationRoot, brandAuthNearExpiryDaysThreshold);

    // Create a separate count query
    CriteriaQuery<Long> countCriteriaQuery = criteriaBuilder.createQuery(Long.class);
    Root<BrandAuthorisation> countRoot = countCriteriaQuery.from(BrandAuthorisation.class);

    // Rebuild the predicate for the count query using the new root
    Predicate countPredicate =
        getPredicateForBrandAuthorisationListing(storeId, sellerCode, brandName, status,
            criteriaBuilder, countRoot, brandAuthNearExpiryDaysThreshold);

    // Apply the predicate to the result query
    criteriaQuery.select(brandAuthorisationRoot).where(predicate);
    criteriaQuery.orderBy(criteriaBuilder.desc(brandAuthorisationRoot.get("createdDate")));

    if (Objects.isNull(predicate) || Objects.isNull(countPredicate)) {
      return new PageImpl<>(new ArrayList<>(), pageable, 0);
    }

    // Execute the result query with pagination
    List<BrandAuthorisation> pagingContent = entityManager.createQuery(criteriaQuery)
        .setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
        .setMaxResults(pageable.getPageSize())
        .getResultList();

    // Apply the predicate to the count query and select the count
    countCriteriaQuery.select(criteriaBuilder.count(countRoot)).where(countPredicate);
    Long totalRecords = entityManager.createQuery(countCriteriaQuery).getSingleResult();

    // Return the paginated result
    return new PageImpl<>(pagingContent, pageable, totalRecords);
  }

  @Override
  public List<BrandAuthorisation> findSellerCodesByAuthorisationStatusAndConfiguration(
      String storeId, String status, int configDays) {

    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<BrandAuthorisation> criteriaQuery =
        criteriaBuilder.createQuery(BrandAuthorisation.class);
    Root<BrandAuthorisation> brandAuthorisationRoot = criteriaQuery.from(BrandAuthorisation.class);
    Predicate predicate = getPredicateForBrandAuthorisationNearExpiry(
        storeId, status, criteriaBuilder, brandAuthorisationRoot, configDays);
    criteriaQuery.select(brandAuthorisationRoot)
        .where(predicate)
        .orderBy(criteriaBuilder.asc(brandAuthorisationRoot.get(AUTH_EXPIRE_DATE)));
    return entityManager.createQuery(criteriaQuery).getResultList();
  }

  private Predicate getPredicateForBrandAuthorisationListing(String storeId, String sellerCode,
      String brandName, String status, CriteriaBuilder criteriaBuilder,
      Root<BrandAuthorisation> brandAuthorisationRoot, int brandAuthNearExpiryDaysThreshold) {
    Predicate predicate = criteriaBuilder.equal(brandAuthorisationRoot.get(STORE_ID), storeId);
    predicate =
        criteriaBuilder.and(predicate, criteriaBuilder.equal(brandAuthorisationRoot.get(MARK_FOR_DELETE), false));
    if (StringUtils.isNotBlank(sellerCode)) {
      predicate =
          criteriaBuilder.and(predicate, criteriaBuilder.equal(brandAuthorisationRoot.get(SELLER_CODE), sellerCode));
    }
    if (StringUtils.isNotBlank(status) && BrandAuthorisationStatus.ACTIVE.name().equals(status)) {
      predicate = criteriaBuilder.and(predicate,
        criteriaBuilder.equal(brandAuthorisationRoot.get(AUTH_STATUS),
          BrandAuthorisationStatus.valueOf(status)));
      predicate = criteriaBuilder.and(predicate,
        criteriaBuilder.between(criteriaBuilder.literal(new Date()),
          brandAuthorisationRoot.get(AUTH_START_DATE).as(Date.class),
          brandAuthorisationRoot.get(AUTH_EXPIRE_DATE).as(Date.class)));
    } else if (StringUtils.isNotBlank(status) && BrandAuthorisationStatus.INACTIVE.name()
      .equals(status)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.or(
              criteriaBuilder.lessThan(criteriaBuilder.literal(new Date()),
                  brandAuthorisationRoot.get(AUTH_START_DATE).as(Date.class)),
              criteriaBuilder.equal(brandAuthorisationRoot.get(AUTH_STATUS), BrandAuthorisationStatus.INACTIVE)
          ));
    }
    else if (BrandAuthorisationStatus.EXPIRED.name().equals(status)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.greaterThan(criteriaBuilder.literal(new Date()),
          brandAuthorisationRoot.get(AUTH_EXPIRE_DATE).as(Date.class)));
    }
    else if (BrandAuthorisationStatus.NEAR_EXPIRY.name().equals(status)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.equal(brandAuthorisationRoot.get(AUTH_STATUS),
              BrandAuthorisationStatus.ACTIVE));
      Calendar calendar = Calendar.getInstance();
      calendar.add(Calendar.DATE, brandAuthNearExpiryDaysThreshold);
      Date currDatePlusThreshold = calendar.getTime();
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.greaterThan(criteriaBuilder.literal(currDatePlusThreshold),
              brandAuthorisationRoot.get(AUTH_EXPIRE_DATE).as(Date.class)));
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.lessThan(criteriaBuilder.literal(new Date()),
              brandAuthorisationRoot.get(AUTH_EXPIRE_DATE).as(Date.class)));
    }
    if (StringUtils.isNotEmpty(brandName)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.like(criteriaBuilder.lower(brandAuthorisationRoot.get(BRAND_NAME)),
              ANY_STRING + brandName.toLowerCase() + ANY_STRING));
    }
    return predicate;
  }

  private Predicate getPredicateForBrandAuthorisationNearExpiry(String storeId, String status,
      CriteriaBuilder criteriaBuilder, Root<BrandAuthorisation> brandAuthorisationRoot,
      int configDays) {
    Calendar calendar = Calendar.getInstance();
    Predicate predicate = criteriaBuilder.equal(brandAuthorisationRoot.get(STORE_ID), storeId);
    predicate = criteriaBuilder.and(predicate,
        criteriaBuilder.equal(brandAuthorisationRoot.get(MARK_FOR_DELETE), Boolean.FALSE));
    Predicate authStartDatePredicate =
        criteriaBuilder.lessThan(brandAuthorisationRoot.get(AUTH_START_DATE), calendar.getTime());
    Predicate authExpireDatePredicate =
        criteriaBuilder.greaterThan(brandAuthorisationRoot.get(AUTH_EXPIRE_DATE), calendar.getTime());
    if (StringUtils.isNotBlank(status) && BrandAuthorisationStatus.ACTIVE.name().equals(status)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.equal(brandAuthorisationRoot.get(AUTHORISATION_STATUS),
              BrandAuthorisationStatus.ACTIVE));
      Date today = calendar.getTime();
      today = removeTimeFromDate(today);
      Predicate expiryDatePredicate = criteriaBuilder.disjunction();
      calendar.setTime(today);
      calendar.add(Calendar.DAY_OF_YEAR, configDays);
      Date targetAuthExpireDate = calendar.getTime();
      targetAuthExpireDate = removeTimeFromDate(targetAuthExpireDate);
      Predicate dateMatchPredicate =
          criteriaBuilder.equal(brandAuthorisationRoot.get(AUTH_EXPIRE_DATE), targetAuthExpireDate);
      expiryDatePredicate = criteriaBuilder.or(expiryDatePredicate, dateMatchPredicate);
      predicate = criteriaBuilder.and(predicate, expiryDatePredicate);
    }
    predicate = criteriaBuilder.and(predicate, authStartDatePredicate, authExpireDatePredicate);
    return predicate;
  }

  private Date removeTimeFromDate(Date date) {
    LocalDate localDate = date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    return Date.from(localDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
  }
}
