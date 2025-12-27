package com.gdn.x.mta.distributiontask.dao.api.util;

import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.dto.ProductDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Expression;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.metamodel.SingularAttribute;
import java.util.Date;


/**
 * Created by virajjasani on 17/09/16.
 */
public class ProductDAOHelperTest {

  private static final String CATEGORY_CODE = "categoryCode";
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String MERCHANT_NAME = "merchant1";
  private static final String VENDOR_ID = "v1";
  private static final Date CURRENT_DATE = new Date();
  private ProductDTO productDTO;

  @InjectMocks
  private ProductDAOHelper productDAOHelper;

  @Mock
  private Root<Product> root;

  @Mock
  private CriteriaBuilder criteriaBuilder;

  private CriteriaQuery<?> query;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    productDTO = new ProductDTO();
    productDTO.setCategoryCode(CATEGORY_CODE);
    productDTO.setStoreId(STORE_ID);
    productDTO.setProductCode(PRODUCT_CODE);
    productDTO.setBusinessPartnerName(MERCHANT_NAME);
    productDTO.setVendorId(VENDOR_ID);
    productDTO.setStartDate(CURRENT_DATE);
    productDTO.setEndDate(CURRENT_DATE);
    productDAOHelper.setProductDTO(productDTO);
  }

  @AfterEach
  public void tearDown() throws Exception {
  }

  @Test
   void toPredicateTest() throws Exception {
    productDAOHelper.toPredicate(root, query, criteriaBuilder);
    Mockito.verify(criteriaBuilder, Mockito.times(6))
        .equal((Expression<?>) Mockito.any(), Mockito.anyString());
    Mockito.verify(criteriaBuilder, Mockito.times(4))
        .like((Expression<String>) Mockito.any(), Mockito.anyString());
    Mockito.verify(criteriaBuilder, Mockito.times(4)).lower((Expression<String>) Mockito.any());
    Mockito.verify(criteriaBuilder)
        .between((Expression<Date>) Mockito.any(), (Date) Mockito.any(),
            (Date) Mockito.any());
    Mockito.verify(criteriaBuilder).and(new Predicate[7]);
    Mockito.verify(root, Mockito.times(11))
        .get((SingularAttribute<? super Product, Object>) Mockito.any());
  }

}
