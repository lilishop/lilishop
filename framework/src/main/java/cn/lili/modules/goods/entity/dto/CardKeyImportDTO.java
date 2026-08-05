package cn.lili.modules.goods.entity.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 卡密批量导入行
 *
 * @author Mike
 * @date 2026-07-31
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class CardKeyImportDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    private Integer row;

    private String cardNo;

    private String cardSecret;
}
