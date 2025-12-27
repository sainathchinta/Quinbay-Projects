package com.gdn.partners.product.orchestrator.repository.data.product.level1;

import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import com.gdn.partners.product.orchestrator.entity.product.level1.ProductLevel1;
import com.gdn.partners.product.orchestrator.model.product.level1.ProductLevel1Filter;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import jakarta.persistence.EntityManager;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Expression;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.util.Arrays;
import java.util.HashSet;

public class ProductLevel1RepositoryTest {

  private static final String DEFAULT_STORE_ID = "STORE-ID";
  private static final String DEFAULT_STORE_CODE = "STORE-CODE";
  private static final String DEFAULT_STORE_NAME = "STORE-NAME";
  private static final String DEFAULT_PRODUCT_ID = "PRODUCT-ID";
  private static final String DEFAULT_CODE = "CODE";
  private static final String DEFAULT_NAME = "NAME";
  private static final String DEFAULT_BRAND_CODE = "BRAND-CODE";
  private static final String DEFAULT_BRAND_NAME = "BRAND-NAME";
  private static final String DEFAULT_CATEGORY_CODE = "CATEGORY-CODE";
  private static final String DEFAULT_CATEGORY_NAME = "CATEGORY-NAME";
  private static final String DEFAULT_CREATED_BY = "CREATED-BY";
  private static final String DEFAULT_SORTED_BY = "SORTED-BY";
  private static final Pageable DEFAULT_PAGEABLE = PageRequest.of(0, 1);
  private static final VerificationMode CALLED_TWICE = Mockito.times(2);
  private static final VerificationMode CALLED_THREE_TIMES = Mockito.times(3);
  private static final VerificationMode CALLED_FOUR_TIMES = Mockito.times(4);
  private static final VerificationMode CALLED_FIVE_TIMES = Mockito.times(5);
  private static final VerificationMode CALLED_SIX_TIMES = Mockito.times(6);
  private static final VerificationMode CALLED_SEVEN_TIMES = Mockito.times(7);
  private static final VerificationMode CALLED_TWENTY_THREE_TIMES = Mockito.times(23);

  @Mock
  private EntityManager entityManager;

  @Mock
  private CriteriaBuilder criteriaBuilder;

  @Mock
  private CriteriaQuery criteriaQuery;

  @Mock
  private Root root;

  @Mock
  private Predicate predicate;

  @Mock
  private Expression expression;

  @Mock
  private Order order;

  @Mock
  private Path path;

  @Mock
  private TypedQuery typedQuery;

  @InjectMocks
  private ProductLevel1RepositoryImpl productLevel1RepositoryImpl;

  @BeforeEach
  public void setup() throws Exception {
    MockitoAnnotations.initMocks(this);
    Mockito.when(this.entityManager.getCriteriaBuilder()).thenReturn(this.criteriaBuilder);
    Mockito.when(this.criteriaBuilder.createQuery(Mockito.any(Class.class)))
        .thenReturn(this.criteriaQuery);
    Mockito.when(this.criteriaBuilder.equal(Mockito.any(Expression.class), Mockito.anyString()))
        .thenReturn(this.predicate);
    Mockito.when(this.criteriaBuilder.isNull(Mockito.any(Expression.class)))
        .thenReturn(this.predicate);
    Mockito.when(this.criteriaBuilder.isTrue(Mockito.any(Expression.class)))
        .thenReturn(this.predicate);
    Mockito.when(this.criteriaBuilder.isFalse(Mockito.any(Expression.class)))
        .thenReturn(this.predicate);
    Mockito.when(this.criteriaBuilder.like(Mockito.any(Expression.class), Mockito.anyString()))
        .thenReturn(this.predicate);
    Mockito.when(this.criteriaBuilder.lower(Mockito.any(Expression.class)))
        .thenReturn(this.predicate);
    Mockito.when(this.criteriaBuilder.asc(Mockito.any(Expression.class))).thenReturn(this.order);
    Mockito.when(this.criteriaBuilder.desc(Mockito.any(Expression.class))).thenReturn(this.order);
    Mockito.when(this.criteriaBuilder.and(Mockito.any(Predicate[].class))).thenReturn(this.predicate);
    Mockito.when(this.criteriaBuilder.or(Mockito.any(Predicate[].class))).thenReturn(this.predicate);
    Mockito.when(this.criteriaBuilder.count(Mockito.any(Expression.class)))
        .thenReturn(this.expression);
    Mockito.when(this.path.in(Mockito.anySet())).thenReturn(this.predicate);
    Mockito.when(this.criteriaQuery.from(Mockito.any(Class.class))).thenReturn(this.root);
    Mockito.when(this.criteriaQuery.select(Mockito.any(Expression.class)))
        .thenReturn(this.criteriaQuery);
    Mockito.when(this.criteriaQuery.where(Mockito.any(Expression.class)))
        .thenReturn(this.criteriaQuery);
    Mockito.when(this.criteriaQuery.orderBy(Mockito.any(Order.class)))
        .thenReturn(this.criteriaQuery);
    Mockito.when(this.root.get(Mockito.anyString())).thenReturn(this.path);
    Mockito.when(this.entityManager.createQuery(Mockito.any(CriteriaQuery.class)))
        .thenReturn(this.typedQuery);
    Mockito.when(this.typedQuery.setFirstResult(Mockito.anyInt())).thenReturn(this.typedQuery);
    Mockito.when(this.typedQuery.setMaxResults(Mockito.anyInt())).thenReturn(this.typedQuery);
    Mockito.when(this.typedQuery.getResultList())
        .thenReturn(Arrays.asList(ProductLevel1.builder().build()));
    Mockito.when(this.typedQuery.getSingleResult()).thenReturn(1L);
  }

  @AfterEach
  public void teardown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.entityManager);
    Mockito.verifyNoMoreInteractions(this.criteriaBuilder);
    Mockito.verifyNoMoreInteractions(this.criteriaQuery);
    Mockito.verifyNoMoreInteractions(this.root);
    Mockito.verifyNoMoreInteractions(this.predicate);
    Mockito.verifyNoMoreInteractions(this.expression);
    Mockito.verifyNoMoreInteractions(this.order);
    Mockito.verifyNoMoreInteractions(this.path);
    Mockito.verifyNoMoreInteractions(this.typedQuery);
  }

  @Test
  public void findByStoreIdAndFilterAndMarkForDeleteFalse_Valid_Success() throws Exception {
    this.productLevel1RepositoryImpl
        .findByStoreIdAndFilterAndMarkForDeleteFalse(ProductLevel1RepositoryTest.DEFAULT_STORE_ID,
            ProductLevel1Filter.builder().storeCode(ProductLevel1RepositoryTest.DEFAULT_STORE_CODE)
                .storeName(ProductLevel1RepositoryTest.DEFAULT_STORE_NAME).productIds(
                new HashSet<>(Arrays.asList(ProductLevel1RepositoryTest.DEFAULT_PRODUCT_ID)))
                .code(ProductLevel1RepositoryTest.DEFAULT_CODE)
                .name(ProductLevel1RepositoryTest.DEFAULT_NAME)
                .brandCode(ProductLevel1RepositoryTest.DEFAULT_BRAND_CODE)
                .brandName(ProductLevel1RepositoryTest.DEFAULT_BRAND_NAME)
                .categoryCode(ProductLevel1RepositoryTest.DEFAULT_CATEGORY_CODE)
                .categoryName(ProductLevel1RepositoryTest.DEFAULT_CATEGORY_NAME)
                .createdBy(ProductLevel1RepositoryTest.DEFAULT_CREATED_BY).states(new HashSet<>(
                Arrays.asList(ProductLevel1State.ACTIVE, ProductLevel1State.DRAFT,
                    ProductLevel1State.IN_PROGRESS, ProductLevel1State.NEED_CORRECTION)))
                .sortedBy(ProductLevel1RepositoryTest.DEFAULT_SORTED_BY).build(),
            ProductLevel1RepositoryTest.DEFAULT_PAGEABLE);
    Mockito.verify(this.entityManager).getCriteriaBuilder();
    Mockito.verify(this.criteriaBuilder, ProductLevel1RepositoryTest.CALLED_TWICE)
        .createQuery(Mockito.any(Class.class));
    Mockito.verify(this.criteriaBuilder, Mockito.times(10))
        .equal(Mockito.any(Expression.class), Mockito.anyString());
    Mockito.verify(this.criteriaBuilder, CALLED_TWICE)
        .equal(Mockito.any(Expression.class), Mockito.eq(false));
    Mockito.verify(this.criteriaBuilder, Mockito.times(6))
        .isNull(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaBuilder, Mockito.times(6))
        .isTrue(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaBuilder, ProductLevel1RepositoryTest.CALLED_SIX_TIMES)
        .isFalse(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaBuilder, Mockito.times(10))
        .like(Mockito.any(Expression.class), Mockito.anyString());
    Mockito.verify(this.criteriaBuilder, Mockito.times(10))
        .lower(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaBuilder).asc(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaBuilder, Mockito.times(8))
        .and(Mockito.any(Predicate[].class));
    Mockito.verify(this.criteriaBuilder, CALLED_TWICE).or(Mockito.any(Predicate[].class));
    Mockito.verify(this.criteriaBuilder).count(Mockito.any(Expression.class));
    Mockito.verify(this.path, ProductLevel1RepositoryTest.CALLED_FOUR_TIMES).in(Mockito.anySet());
    Mockito.verify(this.criteriaQuery, ProductLevel1RepositoryTest.CALLED_TWICE)
        .from(Mockito.any(Class.class));
    Mockito.verify(this.criteriaQuery).select(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaQuery, ProductLevel1RepositoryTest.CALLED_TWICE)
        .where(Mockito.any(Predicate.class));
    Mockito.verify(this.criteriaQuery).orderBy(Mockito.any(Order.class));
    Mockito.verify(this.root, Mockito.times(45))
        .get(Mockito.anyString());
    Mockito.verify(this.entityManager, ProductLevel1RepositoryTest.CALLED_TWICE)
        .createQuery(Mockito.any(CriteriaQuery.class));
    Mockito.verify(this.typedQuery).setFirstResult(Mockito.anyInt());
    Mockito.verify(this.typedQuery).setMaxResults(Mockito.anyInt());
    Mockito.verify(this.typedQuery).getResultList();
    Mockito.verify(this.typedQuery).getSingleResult();
  }

  @Test
  public void findByStoreIdAndFilterAndMarkForDeleteFalse_FilterStatesAreDeletedAndNeedCorrectionAndSortDirectionIsDesc_Success()
      throws Exception {
    this.productLevel1RepositoryImpl
        .findByStoreIdAndFilterAndMarkForDeleteFalse(ProductLevel1RepositoryTest.DEFAULT_STORE_ID,
            ProductLevel1Filter.builder().states(new HashSet<>(
                Arrays.asList(ProductLevel1State.NEED_CORRECTION)))
                .sortedBy(ProductLevel1RepositoryTest.DEFAULT_SORTED_BY)
                .sortDirection(Sort.Direction.DESC).build(),
            ProductLevel1RepositoryTest.DEFAULT_PAGEABLE);
    Mockito.verify(this.entityManager).getCriteriaBuilder();
    Mockito.verify(this.criteriaBuilder, ProductLevel1RepositoryTest.CALLED_TWICE)
        .createQuery(Mockito.any(Class.class));
    Mockito.verify(this.criteriaBuilder, CALLED_TWICE)
        .equal(Mockito.any(Expression.class), Mockito.anyString());
    Mockito.verify(this.criteriaBuilder, CALLED_TWICE)
        .equal(Mockito.any(Expression.class), Mockito.eq(false));
    Mockito.verify(this.criteriaBuilder, CALLED_TWICE).isNull(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaBuilder, ProductLevel1RepositoryTest.CALLED_FOUR_TIMES)
        .isFalse(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaBuilder).desc(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaBuilder, ProductLevel1RepositoryTest.CALLED_FOUR_TIMES)
        .and(Mockito.any(Predicate[].class));
    Mockito.verify(this.criteriaBuilder, CALLED_TWICE).or(Mockito.any(Predicate[].class));
    Mockito.verify(this.criteriaBuilder).count(Mockito.any(Expression.class));
    Mockito.verify(this.path, CALLED_TWICE).in(Mockito.anySet());
    Mockito.verify(this.criteriaQuery, ProductLevel1RepositoryTest.CALLED_TWICE)
        .from(Mockito.any(Class.class));
    Mockito.verify(this.criteriaQuery).select(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaQuery, ProductLevel1RepositoryTest.CALLED_TWICE)
        .where(Mockito.any(Predicate.class));
    Mockito.verify(this.criteriaQuery).orderBy(Mockito.any(Order.class));
    Mockito.verify(this.root, Mockito.times(13)).get(Mockito.anyString());
    Mockito.verify(this.entityManager, ProductLevel1RepositoryTest.CALLED_TWICE)
        .createQuery(Mockito.any(CriteriaQuery.class));
    Mockito.verify(this.typedQuery).setFirstResult(Mockito.anyInt());
    Mockito.verify(this.typedQuery).setMaxResults(Mockito.anyInt());
    Mockito.verify(this.typedQuery).getResultList();
    Mockito.verify(this.typedQuery).getSingleResult();
  }

  @Test
  public void findByStoreIdAndFilterAndMarkForDeleteFalse_FilterStatesAreDeletedAndNeedCorrectionAndSortDirectionIsNull_Success() {
    this.productLevel1RepositoryImpl
        .findByStoreIdAndFilterAndMarkForDeleteFalse(ProductLevel1RepositoryTest.DEFAULT_STORE_ID,
            ProductLevel1Filter.builder().states(new HashSet<>(
                    Arrays.asList(ProductLevel1State.NEED_CORRECTION)))
                .sortedBy(ProductLevel1RepositoryTest.DEFAULT_SORTED_BY)
                .sortDirection(null).build(),
            ProductLevel1RepositoryTest.DEFAULT_PAGEABLE);
    Mockito.verify(this.entityManager).getCriteriaBuilder();
    Mockito.verify(this.criteriaBuilder, ProductLevel1RepositoryTest.CALLED_TWICE)
        .createQuery(Mockito.any(Class.class));
    Mockito.verify(this.criteriaBuilder, CALLED_TWICE)
        .equal(Mockito.any(Expression.class), Mockito.anyString());
    Mockito.verify(this.criteriaBuilder, CALLED_TWICE)
        .equal(Mockito.any(Expression.class), Mockito.eq(false));
    Mockito.verify(this.criteriaBuilder, CALLED_TWICE).isNull(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaBuilder, ProductLevel1RepositoryTest.CALLED_FOUR_TIMES)
        .isFalse(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaBuilder, ProductLevel1RepositoryTest.CALLED_FOUR_TIMES)
        .and(Mockito.any(Predicate[].class));
    Mockito.verify(this.criteriaBuilder, CALLED_TWICE).or(Mockito.any(Predicate[].class));
    Mockito.verify(this.criteriaBuilder).count(Mockito.any(Expression.class));
    Mockito.verify(this.path, CALLED_TWICE).in(Mockito.anySet());
    Mockito.verify(this.criteriaQuery, ProductLevel1RepositoryTest.CALLED_TWICE)
        .from(Mockito.any(Class.class));
    Mockito.verify(this.criteriaQuery).select(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaQuery, ProductLevel1RepositoryTest.CALLED_TWICE)
        .where(Mockito.any(Predicate.class));
    Mockito.verify(this.root, Mockito.times(12)).get(Mockito.anyString());
    Mockito.verify(this.entityManager, ProductLevel1RepositoryTest.CALLED_TWICE)
        .createQuery(Mockito.any(CriteriaQuery.class));
    Mockito.verify(this.typedQuery).setFirstResult(Mockito.anyInt());
    Mockito.verify(this.typedQuery).setMaxResults(Mockito.anyInt());
    Mockito.verify(this.typedQuery).getResultList();
    Mockito.verify(this.typedQuery).getSingleResult();
  }

  @Test
  public void findByStoreIdAndFilterAndMarkForDeleteFalse_FilterStateIsDeletedAndSortedByIsNull_Success()
      throws Exception {
    this.productLevel1RepositoryImpl
        .findByStoreIdAndFilterAndMarkForDeleteFalse(ProductLevel1RepositoryTest.DEFAULT_STORE_ID,
            ProductLevel1Filter.builder()
                .states(new HashSet<>(Arrays.asList(ProductLevel1State.DELETED))).build(),
            ProductLevel1RepositoryTest.DEFAULT_PAGEABLE);
    Mockito.verify(this.entityManager).getCriteriaBuilder();
    Mockito.verify(this.criteriaBuilder, ProductLevel1RepositoryTest.CALLED_TWICE)
        .createQuery(Mockito.any(Class.class));
    Mockito.verify(this.criteriaBuilder, CALLED_TWICE)
        .equal(Mockito.any(Expression.class), Mockito.anyString());
    Mockito.verify(this.criteriaBuilder, CALLED_TWICE)
        .equal(Mockito.any(Expression.class), Mockito.eq(false));
    Mockito.verify(this.criteriaBuilder, ProductLevel1RepositoryTest.CALLED_TWICE)
        .and(Mockito.any(Predicate[].class));
    Mockito.verify(this.criteriaBuilder, CALLED_TWICE).or(Mockito.any(Predicate[].class));
    Mockito.verify(this.criteriaBuilder).count(Mockito.any(Expression.class));
    Mockito.verify(this.path, CALLED_TWICE).in(Mockito.anySet());
    Mockito.verify(this.criteriaQuery, ProductLevel1RepositoryTest.CALLED_TWICE)
        .from(Mockito.any(Class.class));
    Mockito.verify(this.criteriaQuery).select(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaQuery, ProductLevel1RepositoryTest.CALLED_TWICE)
        .where(Mockito.any(Predicate.class));
    Mockito.verify(this.root, Mockito.times(6))
        .get(Mockito.anyString());
    Mockito.verify(this.entityManager, ProductLevel1RepositoryTest.CALLED_TWICE)
        .createQuery(Mockito.any(CriteriaQuery.class));
    Mockito.verify(this.typedQuery).setFirstResult(Mockito.anyInt());
    Mockito.verify(this.typedQuery).setMaxResults(Mockito.anyInt());
    Mockito.verify(this.typedQuery).getResultList();
    Mockito.verify(this.typedQuery).getSingleResult();
  }

  @Test
  public void findByStoreIdAndFilterAndMarkForDeleteFalse_FilterOthersIsNullAndSortedByNotNullAndSortDirectionIsNull_Success()
      throws Exception {
    this.productLevel1RepositoryImpl
        .findByStoreIdAndFilterAndMarkForDeleteFalse(ProductLevel1RepositoryTest.DEFAULT_STORE_ID,
            ProductLevel1Filter.builder().build(),
            ProductLevel1RepositoryTest.DEFAULT_PAGEABLE);
    Mockito.verify(this.entityManager).getCriteriaBuilder();
    Mockito.verify(this.criteriaBuilder, ProductLevel1RepositoryTest.CALLED_TWICE)
        .createQuery(Mockito.any(Class.class));
    Mockito.verify(this.criteriaBuilder, Mockito.times(2))
        .equal(Mockito.any(Expression.class), Mockito.anyString());
    Mockito.verify(this.criteriaBuilder, Mockito.times(2))
        .equal(Mockito.any(Expression.class), Mockito.eq(false));
    Mockito.verify(this.criteriaBuilder, ProductLevel1RepositoryTest.CALLED_TWICE)
        .and(Mockito.any(Predicate[].class));
    Mockito.verify(this.criteriaBuilder).count(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaQuery, ProductLevel1RepositoryTest.CALLED_TWICE)
        .from(Mockito.any(Class.class));
    Mockito.verify(this.criteriaQuery).select(Mockito.any(Expression.class));
    Mockito.verify(this.criteriaQuery, ProductLevel1RepositoryTest.CALLED_TWICE)
        .where(Mockito.any(Predicate.class));
    Mockito.verify(this.root, ProductLevel1RepositoryTest.CALLED_FOUR_TIMES)
        .get(Mockito.anyString());
    Mockito.verify(this.entityManager, ProductLevel1RepositoryTest.CALLED_TWICE)
        .createQuery(Mockito.any(CriteriaQuery.class));
    Mockito.verify(this.typedQuery).setFirstResult(Mockito.anyInt());
    Mockito.verify(this.typedQuery).setMaxResults(Mockito.anyInt());
    Mockito.verify(this.typedQuery).getResultList();
    Mockito.verify(this.typedQuery).getSingleResult();
  }

}