package com.gdn.x.productcategorybase.solr.dao.impl;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcDaoSupport;
import org.springframework.stereotype.Repository;

import com.gdn.x.productcategorybase.solr.dao.ProductDao;
import com.gdn.x.productcategorybase.solr.model.AttributeModel;
import com.gdn.x.productcategorybase.solr.model.ProductModel;

/**
 * Created by Kesha on 24/04/16.
 */
@Repository
public class ProductDaoImpl extends NamedParameterJdbcDaoSupport implements ProductDao {
  private static final String PRODUCT_ID_COLUMN = "product_id";

  private static final String GET_UPC_CODES = "select product_id,upc_code from pcc_product_item " +
      "where product_id in (:ids) and mark_for_delete='f'";
  static final String CATEGORY_MAPPING = "select id, parent_category_id from pcc_category where " +
      "is_activated='t' and mark_for_delete='f'";

  static final String GET_PRODUCT_CATEGORIES = "select pc.category_id,pc.product_id from " +
      "pcc_product_category pc join pcc_product p on (pc.product_id=p.id) where pc" +
      ".mark_for_delete='f' and p.mark_for_delete='f' and p.is_activated='t' and p" +
      ".is_viewable='t' and pc.product_id in" +
      " (:ids)";

  private static final String GET_ATTRIBUTES_FROM_PRODUCT_ID = "SELECT pa.product_attribute_name," +
      "       CASE" +
      "           WHEN pav.descriptive_attribute_value_type = 'SINGLE' THEN pav" +
      ".descriptive_value" +
      "           WHEN pav.descriptive_attribute_value_type = 'PREDEFINED' THEN" +
      "                  (SELECT value" +
      "                   FROM pcc_predefined_allowed_attribute_value" +
      "                   WHERE id = pav.predefined_allowed_attribute_value_id and " +
      " mark_for_delete='f')" +
      "       END attribute_value ," +
      "       pa.product_id" +
      " FROM pcc_product_attribute_value pav," +
      "     pcc_product_attribute pa" +
      " WHERE pav.product_attribute_id =pa.id" +
      "  AND pav.mark_for_delete='f'" +
      "  AND pa.mark_for_delete='f'" +
      "  AND pav.descriptive_attribute_value_type in ('SINGLE','PREDEFINED')" +
      "  AND pa.product_id IN (:ids)";

  static final String GET_ALL_PRODUCTS = "select id,name,product_code,updated_date from pcc_product" +
      " where mark_for_delete='f' and is_activated='t' and is_viewable='t'";

  private static final Logger LOCAL_LOGGER = LoggerFactory.getLogger(ProductDaoImpl.class);

  @Autowired
  public ProductDaoImpl(
      @Qualifier("pcbJdbcTemplate") JdbcTemplate pcbJdbcTemplate) {
    setJdbcTemplate(pcbJdbcTemplate);
  }

  @Override
  public Map<String, String> getAllCategoryMapping() {
    final Map<String, String> categoryMapping = new HashMap<>();
    getJdbcTemplate().query(CATEGORY_MAPPING, new ResultSetExtractor<Object>() {
      @Override
      public Object extractData(ResultSet resultSet) throws SQLException {
        if (resultSet != null) {
          while (resultSet.next()) {
            categoryMapping.put(resultSet.getString("id"), resultSet.getString
                ("parent_category_id"));
          }
        }
        return categoryMapping;
      }
    });
    return categoryMapping;
  }

  @Override
  public Map<String, String> getProductCategories(Set<String> productIdSet) {
    LOCAL_LOGGER.debug("Getting product categories for {} products", productIdSet.size());
    final Map<String, String> productCategoryMap = new HashMap<>();
    MapSqlParameterSource parameterSource = new MapSqlParameterSource();
    parameterSource.addValue("ids", productIdSet);
    getNamedParameterJdbcTemplate().query(GET_PRODUCT_CATEGORIES, parameterSource, new
        ResultSetExtractor<Object>() {
          @Override
          public Object extractData(ResultSet resultSet) throws SQLException  {
            if (resultSet != null) {
              while (resultSet.next()) {
                String key = resultSet.getString(PRODUCT_ID_COLUMN);
                productCategoryMap.put(key, resultSet.getString("category_id"));
              }
            }
            return productCategoryMap;
          }
        });
    return productCategoryMap;
  }

  @Override
  public Map<String, List<AttributeModel>> getProductAttributes(Set<String> productIdSet) {
    LOCAL_LOGGER.debug("Getting product attributes for {} products", productIdSet.size());
    MapSqlParameterSource parameterSource = new MapSqlParameterSource();
    parameterSource.addValue("ids", productIdSet);
    final Map<String, List<AttributeModel>> productAttributes = new HashMap<>();
    getNamedParameterJdbcTemplate().query(GET_ATTRIBUTES_FROM_PRODUCT_ID, parameterSource, new
        ResultSetExtractor<Object>() {
          @Override
          public Object extractData(ResultSet resultSet) throws SQLException {
            if (resultSet != null) {
              while (resultSet.next()) {
                String key = resultSet.getString(PRODUCT_ID_COLUMN);
                AttributeModel model = new AttributeModel(resultSet.getString
                    ("product_attribute_name"), resultSet
                    .getString("attribute_value"));

                if (productAttributes.get(key) == null) {
                  List<AttributeModel> modelList = new ArrayList<>();
                  modelList.add(model);
                  productAttributes.put(key, modelList);
                } else {
                  productAttributes.get(key).add(model);
                }
              }
            }
            return productAttributes;
          }
        });
    return productAttributes;
  }

  @Override
  public List<ProductModel> getAllProducts() {
    return getJdbcTemplate().query(GET_ALL_PRODUCTS, new ProductRowMapper());
  }

  @Override
  public Map<String, Set<String>> getProductToUPCCodesMap(Set<String> productIdSet) {
    LOCAL_LOGGER.debug("Getting product Upc-code list for {} products", productIdSet.size());
    MapSqlParameterSource parameterSource = new MapSqlParameterSource();
    parameterSource.addValue("ids", productIdSet);
    final Map<String, Set<String>> productToUpcCodes = new HashMap<>();
    getNamedParameterJdbcTemplate().query(GET_UPC_CODES, parameterSource, new
        ResultSetExtractor<Object>() {
          @Override
          public Object extractData(ResultSet resultSet) throws SQLException {
            if (resultSet != null) {
              while (resultSet.next()) {
                String key = resultSet.getString(PRODUCT_ID_COLUMN);
                String upcCode = resultSet.getString("upc_code");

                if (productToUpcCodes.get(key) == null) {
                  Set<String> upcCodeList = new HashSet<>();
                  upcCodeList.add(upcCode);
                  productToUpcCodes.put(key, upcCodeList);
                } else {
                  productToUpcCodes.get(key).add(upcCode);
                }
              }
            }
            return productToUpcCodes;
          }
        });
    return productToUpcCodes;
  }

  private class ProductRowMapper implements RowMapper {
    @Override
    public ProductModel mapRow(ResultSet resultSet, int i) throws SQLException {
      return new ProductModel(resultSet.getString("id"), resultSet.getString("name"),
          resultSet.getString("product_code"), resultSet.getDate("updated_date"));
    }
  }

}
