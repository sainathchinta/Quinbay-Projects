package com.gdn.x.productcategorybase.solr.dao.impl;

import com.gdn.x.productcategorybase.solr.model.AttributeModel;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Field;
import java.sql.ResultSet;
import java.util.HashMap;
import org.springframework.jdbc.core.RowMapper;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;


import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by arie.prastowo on 1/23/2017.
 */
public class ProductDaoImplTest {

    @InjectMocks
    private ProductDaoImpl productDaoImpl;

    @Mock
    private JdbcTemplate jdbcTemplate;

    @Mock
    private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

    @Mock
    private ResultSet resultSet;

    @Mock
    private ResultSetExtractor resultSetExtractor;

    @BeforeEach
    public void setup(){
        MockitoAnnotations.initMocks(this);
        Field field = ReflectionUtils.findField(ProductDaoImpl.class, "namedParameterJdbcTemplate");
        field.setAccessible(true);
        ReflectionUtils.setField(field, productDaoImpl, namedParameterJdbcTemplate);
    }

    @Test
    public void test_getAllCategoryMapping() throws Exception {
        Map<String, String> categoryMapping = new HashMap<>();
        Mockito.when(jdbcTemplate.query(anyString(), any(ResultSetExtractor.class)))
            .thenReturn(categoryMapping);
        Map<String, String> resultMapping = productDaoImpl.getAllCategoryMapping();
        Assertions.assertNotNull(resultMapping);

    }

    @Test
    public void test_getProductCategories() {
        Set<String> productIdSet = new HashSet<>();
        String testProductCode = "MTA-12345";
        productIdSet.add(testProductCode);
        ArgumentCaptor<MapSqlParameterSource> paramCaptor =
            ArgumentCaptor.forClass(MapSqlParameterSource.class);
        when(namedParameterJdbcTemplate
            .query(anyString(), paramCaptor.capture(), any(ResultSetExtractor.class)))
            .thenReturn(new Object());
        Map<String, String> result = productDaoImpl.getProductCategories(productIdSet);

        verify(productDaoImpl.getNamedParameterJdbcTemplate()).query(anyString(), paramCaptor.capture(), any(ResultSetExtractor.class));
        Assertions.assertNotNull(result);
        Set<String> capturedProducts = (Set<String>) paramCaptor.getValue().getValue("ids");
        Assertions.assertEquals(1, capturedProducts.size());
        Assertions.assertTrue(capturedProducts.contains(testProductCode));
    }


    @Test
    public void test_getProductAttributes() {
        Set<String> productIdSet = new HashSet<>();
        String testProductCode = "MTA-12345";
        productIdSet.add(testProductCode);
        ArgumentCaptor<MapSqlParameterSource> paramCaptor =
            ArgumentCaptor.forClass(MapSqlParameterSource.class);
        when(namedParameterJdbcTemplate
            .query(anyString(), paramCaptor.capture(), any(ResultSetExtractor.class)))
            .thenReturn(new Object());
        Map<String, List<AttributeModel>> result = productDaoImpl.getProductAttributes(productIdSet);

        verify(productDaoImpl.getNamedParameterJdbcTemplate()).query(anyString(), paramCaptor.capture(), any(ResultSetExtractor.class));
        Assertions.assertNotNull(result);
        Set<String> capturedProducts = (Set<String>) paramCaptor.getValue().getValue("ids");
        Assertions.assertEquals(1, capturedProducts.size());
        Assertions.assertTrue(capturedProducts.contains(testProductCode));
    }


    @Test
    public void test_getProductToUPCCodesMap() {
        Set<String> productIdSet = new HashSet<>();
        String testProductCode = "MTA-12345";
        productIdSet.add(testProductCode);
        ArgumentCaptor<MapSqlParameterSource> paramCaptor =
            ArgumentCaptor.forClass(MapSqlParameterSource.class);
        when(namedParameterJdbcTemplate
            .query(anyString(), paramCaptor.capture(), any(ResultSetExtractor.class)))
            .thenReturn(new Object());

        Map<String, Set<String>> result = productDaoImpl.getProductToUPCCodesMap(productIdSet);

        verify(productDaoImpl.getNamedParameterJdbcTemplate()).query(anyString(), paramCaptor.capture(), any(ResultSetExtractor.class));
        Assertions.assertNotNull(result);
        Set<String> capturedProducts = (Set<String>) paramCaptor.getValue().getValue("ids");
        Assertions.assertEquals(1, capturedProducts.size());
        Assertions.assertTrue(capturedProducts.contains(testProductCode));
    }


    @Test
    public void getAllCategoryMapping() {
        productDaoImpl.getAllCategoryMapping();
        Mockito.verify(jdbcTemplate, Mockito.times(1)).query(Mockito.eq(ProductDaoImpl.CATEGORY_MAPPING),
                Mockito.any(ResultSetExtractor.class));
    }

    @Test
    public void getProductCategories() {
        Map<String, String> result = productDaoImpl.getProductCategories(new HashSet<String>());
        Assertions.assertNotNull(result);
    }

    @Test
    public void getProductAttributes() {
        Map<String, List<AttributeModel>> result = productDaoImpl.getProductAttributes(new HashSet<String>());
        Assertions.assertNotNull(result);
    }

    @Test
    public void getAllProducts() {
        productDaoImpl.getAllProducts();
        Mockito.verify(jdbcTemplate, Mockito.times(1)).query(
                Mockito.eq(ProductDaoImpl.GET_ALL_PRODUCTS),
                Mockito.any(RowMapper.class));
    }

    @Test
    public void getProductToUPCCodesMap() {
        Map<String, Set<String>> result = productDaoImpl.getProductToUPCCodesMap(new HashSet<String>());
        Assertions.assertNotNull(result);
    }
}
