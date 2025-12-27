package model;


import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class IPRProductsEventModel extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = 2137615714182502658L;
  private String productSku;
  private String productCode;
  private String source;
  private Date addedDate;
}