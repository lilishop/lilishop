package cn.lili.modules.file.entity.dto;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.file.entity.FileDirectory;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class FileDirectoryDTO extends FileDirectory{

    @ApiModelProperty(value = "文件目录列表")
    private List<FileDirectory> children= new ArrayList<>();

    public FileDirectoryDTO(FileDirectory fileDirectory){
        BeanUtil.copyProperties(fileDirectory, this);
    }
}
