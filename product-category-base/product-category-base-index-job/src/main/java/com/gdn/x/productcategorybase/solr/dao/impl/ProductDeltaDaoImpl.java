package com.gdn.x.productcategorybase.solr.dao.impl;

import com.gdn.x.productcategorybase.solr.dao.ProductDeltaDao;
import com.gdn.x.productcategorybase.solr.model.ActionType;
import com.gdn.x.productcategorybase.solr.model.DeltaProduct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcDaoSupport;
import org.springframework.stereotype.Repository;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;

/**
 * Created by Kesha on 27/04/16.
 */
@Repository
public class ProductDeltaDaoImpl extends NamedParameterJdbcDaoSupport implements ProductDeltaDao {
  private String GET_DELTA_PRODUCTS = "select product_id id, product_code, product_name ,"
      + "event_type, updated_date from solr_delta_product";

  private String UPDATE_TO_PROCESS_STATE = "update solr_delta_product set status='t' " +
      "where " +
      "product_id in (:ids)";

  private String DELETE_PROCESSED_ROWS = "delete from solr_delta_product where " +
      "status='t'";

  private String UPDATE_ALL_PRODUCTS_TO_PROCESSING = "update solr_delta_product set " +
      "status='t'";

  @Autowired
  public ProductDeltaDaoImpl(
      @Qualifier("pcbJdbcTemplate") JdbcTemplate pcbJdbcTemplate) {
    setJdbcTemplate(pcbJdbcTemplate);
  }

  @Override
  public List<DeltaProduct> fetchUpdatedProducts() {
    return getNamedParameterJdbcTemplate().query(GET_DELTA_PRODUCTS, new DeltaProductRowMapper());
  }

  @Override
  public int updateElementStateToProcessing(List<String> productItemIds) {
    MapSqlParameterSource parameterSource = new MapSqlParameterSource();
    parameterSource.addValue("ids", productItemIds);
    return getNamedParameterJdbcTemplate().update(UPDATE_TO_PROCESS_STATE, parameterSource);
  }

  @Override
  public void deleteProcessedElements() {
    getNamedParameterJdbcTemplate().update(DELETE_PROCESSED_ROWS, new HashMap<String, Object>());
  }

  @Override
  public void updateAllElementsToProcessing() {
    getNamedParameterJdbcTemplate().update(UPDATE_ALL_PRODUCTS_TO_PROCESSING, new HashMap<String,
        Object>());
  }

  private class DeltaProductRowMapper implements RowMapper<DeltaProduct> {
    @Override
    public DeltaProduct mapRow(ResultSet resultSet, int i) throws SQLException {
      return new DeltaProduct(resultSet.getString("id"), resultSet.getString("product_name"),
          resultSet.getString("product_code"), ActionType.valueOf(resultSet.getString
          ("event_type")), resultSet.getDate("updated_date"));
    }
  }
}
