package com.gdn.x.productcategorybase.customrepository.impl;

import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.dto.CustomCategoryDto;
import com.gdn.x.productcategorybase.customrepository.CategoryDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Repository;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

/**
 * Created by virajjasani on 25/07/16.
 */
@Repository("categoryDAO")
public class CategoryDAOImpl implements CategoryDAO {

  private static final String ID = "id";
  private static final String CATEGORY_CODE = "category_code";
  private static final String NAME = "name";
  private static final String PARENT_CATEGORY_ID = "parent_category_id";
  private static final String SEQUENCE = "sequence";
  private static final String IS_ACTIVATED = "is_activated";

  String QUERY_RETRIEVE_CATEGORIES = "select pc.id, pc.category_code, pc.name, pc.is_activated, "
      + "pc.parent_category_id, pc.sequence from pcc_category pc where pc.catalog_id in "
      + "(select c.id from pcc_catalog c where c.store_id=:storeId and c.catalog_type=:catalogType "
      + "and c.mark_for_delete = FALSE ) and pc.mark_for_delete = FALSE  ";

  @Autowired
  private NamedParameterJdbcTemplate namedJdbcTemplate;

  @Override
  public List<CustomCategoryDto> getCategoriesFromCatalogType(String storeId,
      CatalogType catalogType) throws Exception {
    MapSqlParameterSource mapSqlParameterSource = new MapSqlParameterSource();
    mapSqlParameterSource.addValue("catalogType", catalogType.toString());
    mapSqlParameterSource.addValue("storeId", storeId);
    return this.namedJdbcTemplate.query(QUERY_RETRIEVE_CATEGORIES, mapSqlParameterSource,
        new RowMapper<CustomCategoryDto>() {
          @Override
          public CustomCategoryDto mapRow(ResultSet rs, int rowNum) throws SQLException {
            CustomCategoryDto categoryDto = new CustomCategoryDto();
            categoryDto.setId(rs.getString(ID));
            categoryDto.setCategoryCode(rs.getString(CATEGORY_CODE));
            categoryDto.setName(rs.getString(NAME));
            categoryDto.setActivated(rs.getBoolean(IS_ACTIVATED));
            categoryDto.setParentCategoryId(rs.getString(PARENT_CATEGORY_ID));
            categoryDto.setSequence(rs.getInt(SEQUENCE));
            return categoryDto;
          }
        });
  }
}
