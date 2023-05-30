package cn.lili.modules.permission.entity.vo;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.permission.entity.dos.Menu;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * 菜单VO 展示模型
 *
 * @author Chopper
 * @since 2020/11/20 15:38
 */

@Data
public class MenuVO extends Menu {

    @ApiModelProperty(value = "子菜单")
    private List<MenuVO> children = new ArrayList<>();

    public MenuVO() {

    }

    public MenuVO(Menu menu) {
        BeanUtil.copyProperties(menu, this);
    }

    public List<MenuVO> getChildren() {
        if (children != null) {
            children.sort(new Comparator<MenuVO>() {
                @Override
                public int compare(MenuVO o1, MenuVO o2) {
                    return o1.getSortOrder().compareTo(o2.getSortOrder());
                }
            });
            return children;
        }
        return null;
    }
}
